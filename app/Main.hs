module Main where

import Control.Monad
import Control.Monad.Cont (MonadIO (liftIO))
import Data.List (intercalate)
import Data.Text qualified as T
import Options.Applicative
import Parser hiding (expr)
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)
import Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)
import WASMEmitter

data Options = Options
    { input :: Maybe FilePath
    , output :: Maybe FilePath
    , debug :: Bool
    }

optionsParser :: Options.Applicative.Parser Options
optionsParser =
    Options
        <$> optional
            ( strOption
                ( long "input"
                    <> short 'i'
                    <> metavar "FILE"
                    <> help "Input file. Use - for stdin."
                )
            )
        <*> optional
            ( strOption
                ( long "output"
                    <> short 'o'
                    <> metavar "FILE"
                    <> help "Output file. Use - for stdout."
                )
            )
        <*> switch
            ( long "debug"
                <> short 'd'
                <> help "Enable debug mode"
            )

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
    Options input output debug <-
        execParser $
            info
                (optionsParser <**> helper)
                ( fullDesc
                    <> progDesc "Compile a Prisma program to WebAssembly"
                    <> header "prisma - a functional programming language"
                )
    inputContents <- case input of
        Just "-" -> getContents
        Just file -> readFile file
        Nothing -> error "No input file specified"
    let parseResult = parseProgram (T.pack inputContents)
    when debug $ case parseResult of
        Left err -> putStrLn $ "Parse error: " ++ errorBundlePretty err
        Right expr -> putStrLn $ prettyPrintProgram expr
    case parseResult of
        Left err -> putStrLn $ "Parse error: " ++ errorBundlePretty err
        Right expr -> do
            wat <- compileProgramToWAST expr
            case output of
                Just "-" -> putStrLn wat
                Just file -> writeFile file wat
                Nothing -> putStrLn wat
