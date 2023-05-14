{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Analyzer (analyseProgram) where

import Control.Monad (unless, when)
import Data.List (intercalate, partition, elemIndex)
import Parser (Expr (..), Program (..))
import Debug.Trace (trace)

isFuncDef :: Expr -> Bool
isFuncDef (FuncDef{}) = True
isFuncDef _ = False

isFuncDec :: Expr -> Bool
isFuncDec (FuncDec _ _) = True
isFuncDec _ = False

isExternDec :: Expr -> Bool
isExternDec (ExternDec{}) = True
isExternDec _ = False

isFuncCall :: Expr -> Bool
isFuncCall (FuncCall _ _) = True
isFuncCall _ = False

getType :: Expr -> String
getType (Var _) = "Int"
getType (BoolLit _) = "Bool"
getType (IntLit _) = "Int"
getType (StringLit _) = "String"
getType (DoBlock exprs) = getType $ last exprs
getType (BinOp{}) = "Int"
getType (If _ thenExpr _) = getType thenExpr
getType (Let _ _ body) = getType body
getType (FuncDef _ _ body) = getType body
getType (FuncDec _ types) = last types
getType (FuncCall name _) = name
getType (ExternDec _ _ types) = last types
getType (Add a b) = getType a
getType (Sub a b) = getType a
getType (Mul a b) = getType a
getType (Div a b) = getType a
getType (Eq a b) = "Bool"
getType (Neq a b) = "Bool"
getType (Lt a b) = "Bool"
getType (Gt a b) = "Bool"
getType (Le a b) = "Bool"
getType (Ge a b) = "Bool"
getType (And a b) = "Bool"
getType (Or a b) = "Bool"
getType (Not a) = "Bool"

warning :: String -> String
warning msg = "warn: " ++ msg

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : xs) = Just x

analyseProgram :: Program -> String -> String
analyseProgram (Program exprs) targetLanguage = do
  intercalate "" (map analyseExpression exprs)
 where
  (funcDecs, rest) = partition isFuncDec exprs
  (externDecs, _) = partition isExternDec rest

  analyseExpression :: Expr -> String
  analyseExpression (Var name) = name
  analyseExpression (BoolLit True) = ""
  analyseExpression (BoolLit False) = ""
  analyseExpression (IntLit n) = show n
  analyseExpression (StringLit s) = show s
  analyseExpression (If cond thenExpr elseExpr) = analyseExpression cond ++ analyseExpression thenExpr ++ analyseExpression elseExpr
  analyseExpression (Let name val body) = analyseExpression val ++ analyseExpression body
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
  analyseExpression (FuncCall name args) = do
    let argTypes = map (\arg -> case arg of
          FuncCall name _ -> getFunctionType arg
          _ -> getType arg) args
    let funcTypes = getFuncType name
    when (length argTypes /= length funcTypes - 1) (error ("Function call to function with wrong number of arguments: " ++ name))
    unless (all (\(argType, funcType) -> argType == funcType || funcType == "Nothing" || argType == "Any") (zip argTypes funcTypes)) (error ("Function call to function with wrong argument types: " ++ name ++ "\nFormal types: " ++ intercalate ", " funcTypes ++ "\nActual types: " ++ intercalate ", " argTypes))
    ""
    where
      getFuncType name = do
        let funcDec = head' $ filter (\(FuncDec name' _) -> name == name') funcDecs
        let externDec = head' $ filter (\(ExternDec _ name' _) -> name == name') externDecs
        case funcDec of
              Just (FuncDec _ types) -> types
              _ -> case externDec of
                Just (ExternDec _ _ types) -> types
                _ -> error "Impossible"
      getFunctionType (FuncCall name _) = last $ getFuncType name
      getFunctionType _ = error "Can't get type of non-function call"
  analyseExpression (DoBlock exprs) = intercalate "" (map analyseExpression exprs)
  analyseExpression (ExternDec lang name types) = do
    -- FIXME: jsrev unless (lang == targetLanguage) (error ("Extern for wrong language: " ++ lang))
    -- "// extern " ++ lang ++ " " ++ name ++ " :: " ++ intercalate " -> " types
    ""
  analyseExpression _ = ""
