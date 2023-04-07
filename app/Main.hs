import Analyzer
import Data.List (intercalate)
import Parser hiding (expr)

prettyPrintProgram :: Program -> String
prettyPrintProgram (Program expr) = do
  intercalate "\n" $ map show expr

main :: IO ()
main = do
  input <- getContents
  case parseProgram input of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right expr -> putStrLn $ "AST:\n" ++ prettyPrintProgram expr
  -- _ <- error "breakpoint"
  putStrLn "\nAnalysis:"
  putStrLn $
    analyseProgram
      ( case parseProgram input of
          Left err -> error $ "Parse error: " ++ show err
          Right expr -> expr
      )
      "javascript"
