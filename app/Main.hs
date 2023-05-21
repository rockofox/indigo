import Analyzer
import Control.Monad
import Data.List (intercalate)
import Data.Text qualified as T
import JSEmitter (compileProgramToJS)
import Parser hiding (expr)
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)
import Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)
import WASMEmitter

prettyPrintExpr :: Expr -> Int -> String
prettyPrintExpr (DoBlock exprs) i = indent i ++ "DoBlock[\n" ++ intercalate "\n" (map (\x -> prettyPrintExpr x (i + 1)) exprs) ++ "\n" ++ indent i ++ "]"
prettyPrintExpr (FuncDef _ _ expr) i =
  case expr of
    DoBlock _ -> "FuncDef[\n" ++ prettyPrintExpr expr (i + 1) ++ "\n" ++ indent i ++ "]"
    _ -> "FuncDef[" ++ prettyPrintExpr expr 0 ++ "]"
prettyPrintExpr x i = indent i ++ show x

indent :: Int -> [Char]
indent i = replicate (i * 2) ' '

prettyPrintProgram :: Program -> String
prettyPrintProgram (Program exprs) = do
  intercalate "\n" (map (`prettyPrintExpr` 0) exprs)

main :: IO ()
main = do
  input <- getContents
  let parseResult = parseProgram (T.pack input)
  case parseResult of
    Left err -> putStrLn $ "Parse error: " ++ errorBundlePretty err
    Right expr -> do
      isTTY <- queryTerminal stdOutput
      when isTTY $ do
        putStrLn $ "\ESC[32mAST\ESC[0m\n" ++ prettyPrintProgram expr
        putStrLn "\n\ESC[32mAnalysis\ESC[0m"
        putStrLn $ analyseProgram expr "javascript"
      -- putStrLn "\n\ESC[32mJS\ESC[0m"
      -- putStrLn $ compileProgramToJS expr
      putStrLn $ compileProgramToWAST expr
