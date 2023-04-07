module JSEmitter where

import Data.List (intercalate)
import Parser hiding (expr)

-- compileProgramToJS :: Program -> String
-- compileProgramToJS (Program expr) = do
--   intercalate "\n" $ map compileExprToJS expr
--   where
--     compileExprToJS :: Expr -> String
--     compileExprToJS (FuncDec name types) = do
--       "function " ++ name ++ "(" ++ intercalate ", " (map compileTypeToJS types) ++ ") {"
