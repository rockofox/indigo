module RustEmitter where

import Data.List (intercalate)
import Parser

compileToRust :: Expr -> String
compileToRust (Var name) = name
compileToRust (BoolLit True) = "true"
compileToRust (BoolLit False) = "false"
compileToRust (IntLit n) = show n
compileToRust (BinOp op left right) = "(" ++ compileToRust left ++ " " ++ op ++ " " ++ compileToRust right ++ ")"
compileToRust (If cond thenExpr elseExpr) =
  "if "
    ++ compileToRust cond
    ++ " {\n"
    ++ compileToRust thenExpr
    ++ "\n} else {\n"
    ++ compileToRust elseExpr
    ++ "\n}"
compileToRust (Let name val body) =
  "let "
    ++ name
    ++ " = "
    ++ compileToRust val
    ++ ";\n"
    ++ compileToRust body
compileToRust (FuncDef name args body) =
  "fn "
    ++ name
    ++ "("
    ++ intercalate ", " args
    ++ ") -> i32 {\n"
    ++ compileToRust body
    ++ "\n}"
compileToRust (FuncCall name args) = name ++ "(" ++ intercalate ", " (map compileToRust args) ++ ")"
compileToRust (DoBlock exprs) = intercalate "\n" (map compileToRust exprs)

compileProgramToRust :: Program -> String
compileProgramToRust (Program exprs) = intercalate "\n" (map compileToRust exprs)
