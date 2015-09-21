module IRTS.CodegenPHP(codegenPHP) where

import IRTS.CodegenCommon
import IRTS.Lang
import IRTS.Simplified
import Idris.Core.TT

import Data.Maybe
import Data.Char

codegenPHP :: CodeGenerator
codegenPHP ci = do let out = concatMap doCodegen (simpleDecls ci)
                   writeFile (outputFile ci) ("<?php\n" ++ helpers ++ "\n" ++
                                                        out ++ "\n" ++ 
                                                        start ++ "\n" ++ 
                                              "\n\n")

start = phpname (sMN 0 "runMain") ++ "();"

helpers = errCode ++ "\n" ++ 
          doEcho ++ "\n" ++
          doRead ++ "\n" ++
          mkStr ++ "\n" ++
          doAppend ++ "\n"

errCode = "function error($str) { echo \"$str\\n\"; exit(0); }"
doEcho = "function idris_writeStr($str) { echo \"$str\"; }"
doRead = "function idris_readStr() { return fgets(STDIN); }"
doAppend = "function idris_append($l, $r) { return ($l . $r); }"
mkStr = "function mkStr($l) { return array($l); }"

phpname :: Name -> String
phpname n = "idris_" ++ concatMap phpchar (showCG n)
  where phpchar x | isAlpha x || isDigit x = [x]
                  | otherwise = "_" ++ show (fromEnum x) ++ "_"

var :: Name -> String
var n = "$" ++ phpname n

loc :: Int -> String
loc i = "$loc" ++ show i

doCodegen :: (Name, SDecl) -> String
doCodegen (n, SFun _ args i def) = cgFun n args def

cgFun :: Name -> [Name] -> SExp -> String
cgFun n args def 
    = "function " ++ phpname n ++ "("
                  ++ showSep "," (map (loc . fst) (zip [0..] args)) ++ ") {\n"
                  ++ cgBody doRet def ++ "\n}\n\n"
  where doRet :: String -> String -- Return the calculated expression
        doRet str = "return " ++ str ++ ";"

-- cgBody converts the SExp into a chunk of php which calculates the result
-- of an expression, then runs the function on the resulting bit of code.
--
-- We do it this way because we might calculate an expression in a deeply nested
-- case statement, or inside a let, etc, so the assignment/return of the calculated
-- expression itself may happen quite deeply.

cgBody :: (String -> String) -> SExp -> String
cgBody ret (SV (Glob n)) = ret $ phpname n ++ "()"
cgBody ret (SV (Loc i)) = ret $ loc i 
cgBody ret (SApp _ f args) = ret $ phpname f ++ "(" ++ 
                                   showSep "," (map cgVar args) ++ ")"
cgBody ret (SLet (Loc i) v sc)
   = cgBody (\x -> loc i ++ " = " ++ x ++ ";\n") v ++
     cgBody ret sc
cgBody ret (SUpdate n e)
   = cgBody ret e
cgBody ret (SProj e i)
   = ret $ cgVar e ++ "[" ++ show (i + 1) ++ "]"
cgBody ret (SCon _ t n args)
   = ret $ "array(" ++ showSep "," 
              (show t : (map cgVar args)) ++ ")"
cgBody ret (SCase _ e alts)
   = let scrvar = cgVar e 
         scr = if any conCase alts then scrvar ++ "[0]" else scrvar in
       "switch(" ++ scr ++ ") {\n"
         ++ showSep "\nbreak;\n" (map (cgAlt ret scrvar) alts) ++ "\n}"
  where conCase (SConCase _ _ _ _ _) = True
        conCase _ = False
cgBody ret (SChkCase e alts)
   = let scrvar = cgVar e 
         scr = if any conCase alts then scrvar ++ "[0]" else scrvar in
       "switch(" ++ scr ++ ") {\n"
         ++ showSep "\nbreak;\n" (map (cgAlt ret scrvar) alts) ++ "\n}"
  where conCase (SConCase _ _ _ _ _) = True
        conCase _ = False
cgBody ret (SConst c) = ret $ cgConst c
cgBody ret (SOp op args) = ret $ cgOp op (map cgVar args)
cgBody ret SNothing = ret "0"
cgBody ret (SError x) = ret $ "error( " ++ show x ++ ")"
cgBody ret _ = ret $ "error(\"NOT IMPLEMENTED!!!!\")"

cgAlt :: (String -> String) -> String -> SAlt -> String
cgAlt ret scr (SConstCase t exp)
   = "case " ++ show t ++ ":\n" ++ cgBody ret exp
cgAlt ret scr (SDefaultCase exp) = "default:\n" ++ cgBody ret exp
cgAlt ret scr (SConCase lv t n args exp)
   = "case " ++ show t ++ ":\n"
             ++ project 1 lv args ++ "\n" ++ cgBody ret exp
   where project i v [] = ""
         project i v (n : ns) = loc v ++ " = " ++ scr ++ "[" ++ show i ++ "]; "
                                  ++ project (i + 1) (v + 1) ns

cgVar :: LVar -> String
cgVar (Loc i) = loc i 
cgVar (Glob n) = var n

cgConst :: Const -> String
cgConst (I i) = show i
cgConst (Ch i) = show (ord i) -- Treat Char as ints, because PHP treats them as Strings...
cgConst (BI i) = show i
cgConst (Str s) = show s
cgConst TheWorld = "0"
cgConst x | isTypeConst x = "0"
cgConst x = error $ "Constant " ++ show x ++ " not compilable yet"

cgOp :: PrimFn -> [String] -> String
cgOp (LPlus (ATInt _)) [l, r] 
     = "(" ++ l ++ " + " ++ r ++ ")"
cgOp (LMinus (ATInt _)) [l, r] 
     = "(" ++ l ++ " - " ++ r ++ ")"
cgOp (LTimes (ATInt _)) [l, r] 
     = "(" ++ l ++ " * " ++ r ++ ")"
cgOp (LEq (ATInt _)) [l, r] 
     = "(" ++ l ++ " == " ++ r ++ ")"
cgOp (LSLt (ATInt _)) [l, r] 
     = "(" ++ l ++ " < " ++ r ++ ")"
cgOp (LSLe (ATInt _)) [l, r] 
     = "(" ++ l ++ " <= " ++ r ++ ")"
cgOp (LSGt (ATInt _)) [l, r] 
     = "(" ++ l ++ " > " ++ r ++ ")"
cgOp (LSGe (ATInt _)) [l, r] 
     = "(" ++ l ++ " >= " ++ r ++ ")"
cgOp LStrEq [l,r] = "(" ++ l ++ " == " ++ r ++ ")"
cgOp LStrRev [x] = "strrev(" ++ x ++ ")"
cgOp LStrLen [x] = "strlen(utf8_decode(" ++ x ++ "))"
cgOp LStrHead [x] = "ord(" ++ x ++ "[0])"
cgOp LStrIndex [x, y] = "ord(" ++ x ++ "[" ++ y ++ "])"
cgOp LStrTail [x] = "substr(" ++ x ++ ", 1)"

cgOp (LIntStr _) [x] = "\"" ++ x ++ "\""
cgOp (LChInt _) [x] = x
cgOp (LIntCh _) [x] = x
cgOp (LSExt _ _) [x] = x
cgOp (LTrunc _ _) [x] = x
cgOp LWriteStr [_,str] = "idris_writeStr(" ++ str ++ ")"
cgOp LReadStr [_] = "idris_readStr()"
cgOp LStrConcat [l,r] = "idris_append(" ++ l ++ ", " ++ r ++ ")"
cgOp LStrCons [l,r] = "idris_append(chr(" ++ l ++ "), " ++ r ++ ")"
cgOp (LStrInt _) [x] = x
cgOp op exps = "error(\"OPERATOR " ++ show op ++ " NOT IMPLEMENTED!!!!\")"
   -- error("Operator " ++ show op ++ " not implemented")



