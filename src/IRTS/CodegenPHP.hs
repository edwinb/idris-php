module IRTS.CodegenPHP(codegenPHP) where

import IRTS.CodegenCommon
import IRTS.Defunctionalise
import IRTS.Simplified
import Idris.Core.TT

import Data.Maybe
import Data.Char

codegenPHP :: CodeGenerator
codegenPHP ci = do let out = concatMap (doCodegen (getTags (defunDecls ci))) 
                                       (simpleDecls ci)
                   writeFile (outputFile ci) ("<?php\n" ++ helpers ++ "\n" ++
                                                        out ++ "\n" ++ 
                                                        start ++ "\n" ++ 
                                              "\n\n")

start = phpname (sMN 0 "runMain") ++ "();"

helpers = errCode ++ "\n" ++ 
          doEcho ++ "\n" ++
          mkStr ++ "\n" ++
          doAppend ++ "\n"

errCode = "function error($str) { echo \"$str\\n\"; exit(0); }"
doEcho = "function idris_writeStr($str) { echo \"$str\\n\"; }"
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

getTags :: [(Name, DDecl)] -> [(Name, Int)]
getTags = mapMaybe getTag
  where
    getTag (n, DConstructor _ t _) = Just (n, t)
    getTag _ = Nothing

doCodegen :: [(Name, Int)] -> (Name, SDecl) -> String
doCodegen tags (n, SFun _ args i def) = cgFun tags n args def

cgFun :: [(Name, Int)] -> Name -> [Name] -> SExp -> String
cgFun tags n args def 
    = "function " ++ phpname n ++ "("
                  ++ showSep "," (map (loc . fst) (zip [0..] args)) ++ ") {\n"
                  ++ cgBody doRet tags def ++ "\n}\n\n"

cgBody :: (String -> String) -> [(Name, Int)] -> SExp -> String
cgBody ret tags (SV (Glob n)) = ret $ phpname n ++ "()"
cgBody ret tags (SV (Loc i)) = ret $ loc i 
cgBody ret tags (SApp _ f args)
   = case lookup f tags of
          Just i -> cgBody ret tags (SCon Nothing i f args)
          _ -> ret $ phpname f ++ "(" ++ 
                     showSep "," (map cgVar args) ++ ")"
cgBody ret tags (SLet (Loc i) v sc)
   = cgBody (\x -> loc i ++ " = " ++ x ++ ";\n") tags v ++
     cgBody ret tags sc
cgBody ret tags (SUpdate n e)
   = cgBody ret tags e
cgBody ret tags (SProj e i)
   = ret $ cgVar e ++ "[" ++ show (i + 1) ++ "]"
cgBody ret tags (SCon _ t n args)
   = ret $ "array(" ++ showSep "," 
              (show t : (map cgVar args)) ++ ")"
cgBody ret tags (SCase _ e alts)
   = let scrvar = cgVar e 
         scr = if any conCase alts then scrvar ++ "[0]" else scrvar in
       "switch(" ++ scr ++ ") {\n"
         ++ showSep "\nbreak;\n" (map (cgAlt ret tags scrvar) alts) ++ "\n}"
  where conCase (SConCase _ _ _ _ _) = True
        conCase _ = False
cgBody ret tags (SChkCase e alts)
   = let scrvar = cgVar e 
         scr = if any conCase alts then scrvar ++ "[0]" else scrvar in
       "switch(" ++ scr ++ ") {\n"
         ++ showSep "\nbreak;\n" (map (cgAlt ret tags scrvar) alts) ++ "\n}"
  where conCase (SConCase _ _ _ _ _) = True
        conCase _ = False
cgBody ret tags (SConst c) = ret $ cgConst c
cgBody ret tags (SOp op args) = ret $ cgOp op (map cgVar args)
cgBody ret tags SNothing = ret "0"
cgBody ret tags (SError x) = ret $ "error( " ++ x ++ ")"
cgBody ret _ _ = ret $ "error(\"NOT IMPLEMENTED!!!!\")"

doRet :: String -> String
doRet str = "return " ++ str ++ ";"

cgAlt :: (String -> String) -> [(Name, Int)] -> String -> SAlt -> String
cgAlt ret tags scr (SConstCase t exp)
   = "case " ++ show t ++ ":\n" ++ cgBody ret tags exp
cgAlt ret tags scr (SDefaultCase exp) = "default:\n" ++ cgBody ret tags exp
cgAlt ret tags scr (SConCase lv t n args exp)
   = case lookup n tags of
          Just i -> "case " ++ show i ++ ":\n"
                  ++ project 1 lv args ++ "\n" ++ cgBody ret tags exp
          _ -> error "Can't happen"
   where project i v [] = ""
         project i v (n : ns) = loc v ++ " = " ++ scr ++ "[" ++ show i ++ "]; "
                                  ++ project (i + 1) (v + 1) ns

cgVar :: LVar -> String
cgVar (Loc i) = loc i 
cgVar (Glob n) = var n

cgConst :: Const -> String
cgConst (I i) = show i
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
cgOp (LIntStr _) [x] = x
cgOp (LSExt _ _) [x] = x
cgOp (LTrunc _ _) [x] = x
cgOp LWriteStr [_,str] = "idris_writeStr(" ++ str ++ ")"
cgOp LStrConcat [l,r] = "idris_append(" ++ l ++ ", " ++ r ++ ")"
cgOp op exps = "error(\"OPERATOR " ++ show op ++ " NOT IMPLEMENTED!!!!\")"
   -- error("Operator " ++ show op ++ " not implemented")



