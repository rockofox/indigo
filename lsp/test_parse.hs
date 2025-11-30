{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Parser (Expr (..), Program (..), initCompilerFlags, needsMain, parseProgram)

main :: IO ()
main = do
    let source = "struct Person = (name: String, age: Int)\nlet p = Person{name: \"Alice\", age: 30}\nlet n = p.name"
    let flags = initCompilerFlags{needsMain = False}
    case parseProgram (T.pack source) flags of
        Left err -> putStrLn $ "Parse error: " ++ show err
        Right (Program exprs) -> do
            putStrLn $ "Parsed " ++ show (length exprs) ++ " expressions:"
            mapM_ (\e -> putStrLn $ "  - " ++ take 100 (show e)) exprs
