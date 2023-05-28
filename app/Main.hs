module Main where

import Analyzer
import Control.Monad
import Control.Monad.Cont (MonadIO (liftIO))
import Data.List (intercalate)
import Data.Text qualified as T
import JSEmitter (compileProgramToJS)
import Options.Applicative
import Parser hiding (expr)
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)
import Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)
import WASMEmitter

data Options = Options
    { input :: String
    , output :: String
    , target :: String
    }

optionsParser :: Options.Applicative.Parser Options
optionsParser =
    Options
        <$> strOption
            ( long "input"
                <> short 'i'
                <> metavar "FILE"
                <> help "Input file"
            )
        <*> strOption
            ( long "output"
                <> short 'o'
                <> metavar "FILE"
                <> help "Output file"
            )
        <*> strOption
            ( long "target"
                <> short 't'
                <> metavar "TARGET"
                <> help "Compilation target (javascript or wasm)"
            )

main :: IO ()
main = do
    Options input output target <-
        execParser $
            info
                (optionsParser <**> helper)
                ( fullDesc
                    <> progDesc "Compile a Funk program to JavaScript or WebAssembly"
                    <> header "funk - a functional programming language"
                )
    inputContents <- readFile input
    let parseResult = parseProgram (T.pack inputContents)
    case parseResult of
        Left err -> putStrLn $ "Parse error: " ++ errorBundlePretty err
        Right expr -> do
            case target of
                "javascript" -> do
                    let js = compileProgramToJS expr
                    case output of
                        "" -> putStrLn js
                        _ -> writeFile output js
                "wasm" -> do
                    wat <- compileProgramToWAST expr
                    case output of
                        "" -> putStrLn wat
                        _ -> writeFile output wat
                _ -> putStrLn "Invalid target"
