import Analyzer
import Control.Monad
import Data.List (intercalate)
import JSEmitter (compileProgramToJS)
import Parser hiding (expr)
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)

prettyPrintProgram :: Program -> String
prettyPrintProgram (Program expr) = do
  intercalate "\n" $ map show expr

main :: IO ()
main = do
  input <- getContents
  isTTY <- queryTerminal stdOutput
  when isTTY $ do
    case parseProgram input of
      Left err -> putStrLn $ "Parse error: " ++ show err
      Right expr -> putStrLn $ "\ESC[32mAST\ESC[0m\n" ++ prettyPrintProgram expr
    putStrLn "\n\ESC[32mAnalysis\ESC[0m"
    putStrLn $
      analyseProgram
        ( case parseProgram input of
            Left err -> error $ "Parse error: " ++ show err
            Right expr -> expr
        )
        "javascript"
    putStrLn "\n\ESC[32mJS\ESC[0m"
  putStrLn $
    compileProgramToJS
      ( case parseProgram input of
          Left err -> error $ "Parse error: " ++ show err
          Right expr -> expr
      )
