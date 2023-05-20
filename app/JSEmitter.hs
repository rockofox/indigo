module JSEmitter (compileProgramToJS) where

import Data.List (intercalate)
import Parser hiding (expr, exprs, types)

compileProgramToJS :: Program -> String
compileProgramToJS (Program exprs) = do
  intercalate ";\n" (map compileExprToJS exprs) ++ "\nmain();"
  where
    externFunctions = [x | x@(ExternDec language _ _) <- exprs]
    externNormal = map (\(ExternDec _ name _) -> name) functions
      where
        functions = filter (\(ExternDec language _ _) -> language == "js") externFunctions
    externReverse = map (\(ExternDec _ name _) -> name) functions
      where
        functions = filter (\(ExternDec language _ _) -> language == "jsrev") externFunctions
    compileExprToJS :: Expr -> String
    compileExprToJS (Var name) = name
    compileExprToJS (BoolLit True) = "true"
    compileExprToJS (BoolLit False) = "false"
    compileExprToJS (IntLit n) = show n
    compileExprToJS (StringLit s) = show s
    compileExprToJS (BinOp op left right) = "(" ++ compileExprToJS left ++ " " ++ op ++ " " ++ compileExprToJS right ++ ")"
    compileExprToJS (If cond thenExpr elseExpr) =
      "if "
        ++ compileExprToJS cond
        ++ " {\n"
        ++ compileExprToJS thenExpr
        ++ "\n} else {\n"
        ++ compileExprToJS elseExpr
        ++ "\n}"
    compileExprToJS (Let name expr body) =
      "let " ++ name ++ " = " ++ compileExprToJS expr ++ ";\n" ++ compileExprToJS body
    compileExprToJS (DoBlock body) = do
      "{\n" ++ intercalate ";\n" (map compileExprToJS body) ++ ";\n}"
    compileExprToJS (FuncDef name args body)
      | not (null args) = "const " ++ name ++ " = " ++ concatMap (\x -> '(' : x ++ ")=>") args ++ compileExprToJS body
      | otherwise = "const " ++ name ++ " = ()=>" ++ compileExprToJS body
    compileExprToJS (FuncDec name types) =
      "// function " ++ name ++ "(" ++ intercalate ", " types ++ ")"
    compileExprToJS (FuncCall name args)
      | name `elem` externReverse =
          intercalate "," (map compileExprToJS args) ++ "." ++ name ++ "()"
      | name `elem` externNormal =
          name ++ "(" ++ intercalate ", " (map compileExprToJS args) ++ ")"
      | otherwise =
          "(" ++ name ++ concatMap (\x -> '(' : compileExprToJS x ++ ")") args ++ ")"
    compileExprToJS (ExternDec language name types) =
      "// extern " ++ language ++ " function " ++ name ++ "(" ++ intercalate ", " types ++ ")"
    compileExprToJS (Add a b) = compileExprToJS a ++ " + " ++ compileExprToJS b
    compileExprToJS (Sub a b) = compileExprToJS a ++ " - " ++ compileExprToJS b
    compileExprToJS (Mul a b) = compileExprToJS a ++ " * " ++ compileExprToJS b
    compileExprToJS (Div a b) = compileExprToJS a ++ " / " ++ compileExprToJS b
    compileExprToJS (Eq a b) = compileExprToJS a ++ " === " ++ compileExprToJS b
    compileExprToJS (Neq a b) = compileExprToJS a ++ " !== " ++ compileExprToJS b
    compileExprToJS (Lt a b) = compileExprToJS a ++ " < " ++ compileExprToJS b
    compileExprToJS (Gt a b) = compileExprToJS a ++ " > " ++ compileExprToJS b
    compileExprToJS (Le a b) = compileExprToJS a ++ " <= " ++ compileExprToJS b
    compileExprToJS (Ge a b) = compileExprToJS a ++ " >= " ++ compileExprToJS b
    compileExprToJS (And a b) = compileExprToJS a ++ " && " ++ compileExprToJS b
    compileExprToJS (Or a b) = compileExprToJS a ++ " || " ++ compileExprToJS b
    compileExprToJS (Not a) = "!" ++ compileExprToJS a
