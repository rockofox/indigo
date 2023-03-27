{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Analyzer (analyseProgram) where

import Control.Monad (unless)
import Data.List (intercalate, partition)
import Parser

isFuncDef :: Expr -> Bool
isFuncDef (FuncDef {}) = True
isFuncDef _ = False

isFuncDec :: Expr -> Bool
isFuncDec (FuncDec _ _) = True
isFuncDec _ = False

isFuncCall :: Expr -> Bool
isFuncCall (FuncCall _ _) = True
isFuncCall _ = False

analyseProgram :: Program -> String
analyseProgram (Program exprs) = do
  intercalate "\n" (map analyseExpression exprs)
  where
    (funcDecs, _) = partition isFuncDec exprs

    analyseExpression :: Expr -> String
    analyseExpression (Var name) = name
    analyseExpression (BoolLit True) = "true"
    analyseExpression (BoolLit False) = "false"
    analyseExpression (IntLit n) = show n
    analyseExpression (BinOp op left right) = "(" ++ analyseExpression left ++ " " ++ op ++ " " ++ analyseExpression right ++ ")"
    analyseExpression (If cond thenExpr elseExpr) =
      "if "
        ++ analyseExpression cond
        ++ " {\n"
        ++ analyseExpression thenExpr
        ++ "\n} else {\n"
        ++ analyseExpression elseExpr
        ++ "\n}"
    analyseExpression (Let name val body) =
      "let "
        ++ name
        ++ " = "
        ++ analyseExpression val
        ++ ";\n"
        ++ analyseExpression body
    analyseExpression (FuncDef name args body) = do
      unless (any (\(FuncDec name' _) -> name == name') funcDecs) (error ("Function definition for nonexistant function: " ++ name))
      "fn "
        ++ name
        ++ "("
        ++ intercalate ", " args
        ++ ") -> i32 {\n"
        ++ analyseExpression body
        ++ "\n}"
    analyseExpression (FuncDec name types) = "// " ++ name ++ ":" ++ intercalate "," types
    analyseExpression (FuncCall name args) = do
      unless (any (\(FuncDec name' _) -> name == name') funcDecs) (error ("Function call to nonexistant function: " ++ name))
      name ++ "(" ++ intercalate ", " (map analyseExpression args) ++ ")"
    analyseExpression (DoBlock exprs) = intercalate "\n" (map analyseExpression exprs)
