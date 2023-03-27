import Analyzer
import Parser hiding (expr)

main :: IO ()
main = do
  input <- getContents
  case parseProgram input of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right expr -> putStrLn $ "Parsed expression: " ++ show expr
  putStrLn $ analyseProgram $ case parseProgram input of
    Left err -> error $ "Parse error: " ++ show err
    Right expr -> expr
