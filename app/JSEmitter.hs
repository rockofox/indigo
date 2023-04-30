module JSEmitter where

import Data.List (intercalate)
import Parser hiding (expr, types, exprs)

compileProgramToJS :: Program -> String
compileProgramToJS (Program exprs) = do
  intercalate "\n" (map compileExprToJS exprs) ++ "\nmain();"
  where
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
  compileExprToJS (DoBlock exprs) = do
    "{" ++ (intercalate ";\n" $ map compileExprToJS exprs) ++ "}"
  compileExprToJS (FuncDef name args body) =
    "const " ++ name ++ " = (" ++ intercalate ", " args ++ ") => " ++ compileExprToJS body
  compileExprToJS (FuncDec name types) =
    "// function " ++ name ++ "(" ++ intercalate ", " types ++ ")"
  compileExprToJS (FuncCall name args) =
    name ++ "(" ++ intercalate ", " (map compileExprToJS args) ++ ")"
  compileExprToJS (ExternDec language name types) =
    "// extern " ++ language ++ " function " ++ name ++ "(" ++ intercalate ", " types ++ ")"
