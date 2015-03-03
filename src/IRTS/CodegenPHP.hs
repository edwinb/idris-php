module IRTS.CodegenPHP(codegenPHP) where

import IRTS.CodegenCommon
import IRTS.Defunctionalise
import Idris.Core.TT

import Data.Maybe
import Data.Char

codegenPHP :: CodeGenerator
codegenPHP ci = do let out = concatMap (doCodegen (getTags (defunDecls ci))) 
                                       (defunDecls ci)
                   writeFile (outputFile ci) ("<?\n" ++ helpers ++ "\n" ++
                                                        out ++ "\n" ++ 
                                                        start ++ "\n" ++ 
                                              "\n?>\n")

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

getTags :: [(Name, DDecl)] -> [(Name, Int)]
getTags = mapMaybe getTag
  where
    getTag (n, DConstructor _ t _) = Just (n, t)
    getTag _ = Nothing

doCodegen :: [(Name, Int)] -> (Name, DDecl) -> String
doCodegen tags (n, DConstructor _ _ _) = ""
doCodegen tags (n, DFun _ args def) = cgFun tags n args def

cgFun :: [(Name, Int)] -> Name -> [Name] -> DExp -> String
cgFun tags n args def 
    = "function " ++ phpname n ++ "("
                  ++ showSep "," (map var args) ++ ") {\n"
                  ++ cgLets tags args (liftLets def) ++ "\n}\n\n"

liftLets :: DExp -> ([(Name, DExp)], DExp)
liftLets exp = ([], exp) -- TMP

cgLets :: [(Name, Int)] -> [Name] -> ([(Name, DExp)], DExp) -> String
cgLets tags args ([], end) = cgBody True tags args end
cgLets tags args (((n, b) : ns), end) 
    = var n ++ " = " ++ cgBody False tags args b ++ ";\n"
                     ++ cgLets tags args (ns, end)

cgBody :: Bool -> [(Name, Int)] -> [Name] -> DExp -> String
cgBody ret tags env (DV (Glob n)) | n `elem` env = doRet ret $ var n
                                  | otherwise = doRet ret $ phpname n ++ "()"
cgBody ret tags env (DV (Loc i)) = doRet ret $ var (env!!i)
cgBody ret tags env (DApp _ f args)
   = case lookup f tags of
          Just i -> cgBody ret tags env (DC Nothing i f args)
          _ -> doRet ret $ phpname f ++ "(" ++ 
                           showSep "," (map (cgBody False tags env) args) ++ ")"
cgBody True tags env (DLet n v sc)
   = var n ++ " = " ++ cgBody False tags env v ++ ";\n" ++
     cgBody True tags (env ++ [n]) sc
cgBody ret tags env (DUpdate n e)
   = cgBody True tags env e
cgBody ret tags env (DProj e i)
   = doRet ret $ cgBody False tags env e ++ "[" ++ show (i + 1) ++ "]"
cgBody ret tags env (DC _ t n args)
   = doRet ret $ "array(" ++ showSep "," 
                    (show t : (map (cgBody False tags env) args)) ++ ")"
cgBody ret tags env (DCase _ e alts)
   = let scrvar = cgBody False tags env e 
         scr = if any conCase alts then scrvar ++ "[0]" else scrvar in
       "switch(" ++ scr ++ ") {\n"
         ++ showSep "\nbreak;\n" (map (cgAlt tags env scrvar) alts) ++ "\n}"
  where conCase (DConCase _ _ _ _) = True
        conCase _ = False
cgBody ret tags env (DChkCase e alts)
   = let scr = cgBody False tags env e in
     "switch(" ++ cgBody False tags env e ++ "[0]) {\n"
       ++ showSep "\nbreak;\n" (map (cgAlt tags env scr) alts) ++ "\n}"
cgBody ret tags env (DConst c)
   = doRet ret $ cgConst c
cgBody ret tags env (DOp op args) = doRet ret $ cgOp tags env op 
                                       (map (cgBody False tags env) args)
cgBody ret tags env DNothing = doRet ret "0"
cgBody ret tags env (DError x) = doRet ret $ "error( " ++ x ++ ")"
cgBody ret _ _ _ = doRet ret $ "error(\"NOT IMPLEMENTED!!!!\")"

doRet :: Bool -> String -> String
doRet False str = str
doRet True str = "return " ++ str ++ ";"

cgAlt :: [(Name, Int)] -> [Name] -> String -> DAlt -> String
cgAlt tags env scr (DConstCase t exp)
   = "case " ++ show t ++ ":\n" ++ cgBody True tags env exp
cgAlt tags env scr (DConCase t n args exp)
   = case lookup n tags of
          Just i -> "case " ++ show i ++ ":\n"
                        ++ project 1 args ++ "\n" ++ cgBody True tags (env ++ args) exp
          _ -> error "Can't happen"
   where project i [] = ""
         project i (n : ns) = var n ++ " = " ++ scr ++ "[" ++ show i ++ "]; "
                                  ++ project (i + 1) ns
cgAlt tags env scr (DDefaultCase exp) = "default:\n" ++ cgBody True tags env exp

cgConst :: Const -> String
cgConst (I i) = show i
cgConst (BI i) = show i
cgConst (Str s) = show s
cgConst TheWorld = "0"
cgConst x | isTypeConst x = "0"
cgConst x = error $ "Constant " ++ show x ++ " not compilable yet"

cgOp :: [(Name, Int)] -> [Name] -> PrimFn -> [String] -> String
cgOp tags env (LPlus (ATInt _)) [l, r] 
     = "(" ++ l ++ " + " ++ r ++ ")"
cgOp tags env (LMinus (ATInt _)) [l, r] 
     = "(" ++ l ++ " - " ++ r ++ ")"
cgOp tags env (LTimes (ATInt _)) [l, r] 
     = "(" ++ l ++ " * " ++ r ++ ")"
cgOp tags env (LEq (ATInt _)) [l, r] 
     = "(" ++ l ++ " == " ++ r ++ ")"
cgOp tags env (LSLt (ATInt _)) [l, r] 
     = "(" ++ l ++ " < " ++ r ++ ")"
cgOp tags env (LSLe (ATInt _)) [l, r] 
     = "(" ++ l ++ " <= " ++ r ++ ")"
cgOp tags env (LSGt (ATInt _)) [l, r] 
     = "(" ++ l ++ " > " ++ r ++ ")"
cgOp tags env (LSGe (ATInt _)) [l, r] 
     = "(" ++ l ++ " >= " ++ r ++ ")"
cgOp tags env (LIntStr _) [x] = x
cgOp tags env (LSExt _ _) [x] = x
cgOp tags env (LTrunc _ _) [x] = x
cgOp tags env LWriteStr [_,str] = "idris_writeStr(" ++ str ++ ")"
cgOp tags env LStrConcat [l,r] = "idris_append(" ++ l ++ ", " ++ r ++ ")"
cgOp tags env op exps = "error(\"OPERATOR " ++ show op ++ " NOT IMPLEMENTED!!!!\")"
   -- error("Operator " ++ show op ++ " not implemented")



