{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Analyzer (analyseProgram) where

import Control.Monad (unless, when)
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

getType :: Expr -> String
getType (Var _) = "Int"
getType (BoolLit _) = "Bool"
getType (IntLit _) = "Int"
getType (DoBlock exprs) = getType $ last exprs
getType (BinOp {}) = "Int"
getType (If _ thenExpr _) = getType thenExpr
getType (Let _ _ body) = getType body
getType (FuncDef _ _ body) = getType body
getType (FuncDec _ types) = last types
getType (FuncCall _ _) = "Int"

warning :: String -> String
warning msg = "warn: " ++ msg

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
      -- unless (any (\(FuncDec name' types) -> name == name' && length args == length types) funcDecs) (error ("Function definition for function with wrong number of arguments: " ++ name))
      -- Make sure the return type is correct
      let funcDec = head $ filter (\(FuncDec name' _) -> name == name') funcDecs
      let funcTypes = case funcDec of
            FuncDec _ types -> types
            _ -> error "Impossible"
      unless (last funcTypes == getType body || last funcTypes == "Nothing") (error ("Function definition for function with wrong return type: " ++ name))
      -- if last funcTypes == "Nothing" && getType body /= "Nothing"
      --   then warning $ "Implicit cast of function return type from " ++ getType body ++ " to Nothing: " ++ name
      --   else ""
      analyseExpression body
    analyseExpression (FuncDec name types) = "// " ++ name ++ ":" ++ intercalate "," types
    analyseExpression (FuncCall name args) = do
      unless (any (\(FuncDec name' _) -> name == name') funcDecs) (error ("Function call to nonexistant function: " ++ name))
      unless (any (\(FuncDec name' types) -> name == name' && length args == length types) funcDecs) (error ("Function call to function with wrong number of arguments: " ++ name))
      -- Make sure all the types are correct
      let funcDec = head $ filter (\(FuncDec name' _) -> name == name') funcDecs
      let argTypes = map getType' args
      let funcTypes = case funcDec of
            FuncDec _ types -> types
            _ -> error "Impossible"
      unless (all (uncurry (==)) (zip argTypes funcTypes)) (error ("Function call to function with wrong argument types: " ++ name))
      name ++ "(" ++ intercalate ", " (map analyseExpression args) ++ ")"
      where
        getType' :: Expr -> String
        getType' (FuncCall _ _) = do
          let funcDec = head $ filter (\(FuncDec name' _) -> name == name') funcDecs
          case funcDec of
            FuncDec _ types -> last types
            _ -> error "Impossible"
        getType' expr = getType expr
    analyseExpression (DoBlock exprs) = intercalate "\n" (map analyseExpression exprs)
