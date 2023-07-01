module Main where

import CEmitter
import Control.Monad
import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.State (evalStateT)
import Data.ByteString.Lazy qualified as B
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import GHC.IO.Handle (hFlush)
import GHC.IO.StdHandles (stdout)
import Interpreter (fromBytecode, initialState, repl, run, toBytecode)
import Options.Applicative
import Parser hiding (expr)
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)
import Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)
import WASMEmitter

data Target = WASM | C | Bytecode deriving (Show, Eq, Read)

data Options = Options
    { input :: Maybe FilePath
    , output :: Maybe FilePath
    , debug :: Bool
    , verbose :: Bool
    , target :: Maybe Target
    , useInterpreter :: Bool
    , runBytecode :: Bool
    , launchRepl :: Bool
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
        <*> switch (long "verbose" <> short 'v' <> help "Enable verbose mode")
        <*> optional
            ( option
                auto
                ( long "target"
                    <> short 't'
                    <> metavar "TARGET"
                    <> help "Target language. Either 'wasm', 'c', or 'bytecode'. Default is 'c'."
                    <> value C
                    <> showDefault
                )
            )
        <*> switch
            ( long "interpreter"
                <> short 'r'
                <> help "Use the interpreter to run the program"
            )
        <*> switch
            ( long "run-bytecode"
                <> short 'x'
                <> help "Run bytecode from file"
            )
        <*> switch
            ( long "repl"
                <> short 'p'
                <> help "Launch a REPL"
            )

prettyPrintExpr :: Expr -> Int -> String
prettyPrintExpr (DoBlock exprs) i = indent i ++ "DoBlock[\n" ++ intercalate "\n" (map (\x -> prettyPrintExpr x (i + 1)) exprs) ++ "\n" ++ indent i ++ "]"
prettyPrintExpr (FuncDef name _ expr) i =
    case expr of
        DoBlock _ -> "FuncDef[<" ++ name ++ ">\n" ++ prettyPrintExpr expr (i + 1) ++ "\n" ++ indent i ++ "]"
        _ -> "FuncDef[<" ++ name ++ ">" ++ prettyPrintExpr expr 0 ++ "]"
prettyPrintExpr x i = indent i ++ show x

indent :: Int -> [Char]
indent i = replicate (i * 2) ' '

prettyPrintProgram :: Program -> String
prettyPrintProgram (Program exprs) = do
    intercalate "\n" (map (`prettyPrintExpr` 0) exprs)

inputFile :: Maybe [Char] -> IO String
inputFile input = case input of
    Just "-" -> getContents
    Just file -> readFile file
    Nothing -> error "No input file specified"

inputFileBinary :: Maybe [Char] -> IO B.ByteString
inputFileBinary input = case input of
    Just "-" -> B.getContents
    Just file -> B.readFile file
    Nothing -> error "No input file specified"

main :: IO ()
main = do
    Options input output debug verbose target useInterpreter runBytecode launchRepl <-
        execParser $
            info
                (optionsParser <**> helper)
                ( fullDesc
                    <> progDesc "Compile a Prisma program to WebAssembly, C, or bytecode"
                    <> header "prisma - a functional programming language"
                )

    if launchRepl
        then void $ evalStateT repl Interpreter.initialState
        else
            if runBytecode
                then do
                    bytecode <- inputFileBinary input
                    run (fromBytecode bytecode)
                else do
                    i <- inputFile input
                    let expr = case parseProgram (T.pack i) CompilerFlags{verboseMode = verbose} of
                            Left err -> error $ "Parse error: " ++ errorBundlePretty err
                            Right expr -> expr
                    when debug $ putStrLn $ prettyPrintProgram expr
                    if useInterpreter
                        then run expr
                        else do
                            let t = fromMaybe (error "No target specified") target
                            case t of
                                WASM -> do
                                    wat <- compileProgramToWAST expr
                                    case output of
                                        Just "-" -> putStrLn wat
                                        Just file -> writeFile file wat
                                        Nothing -> putStrLn wat
                                C -> do
                                    let c = compileProgramToC expr
                                    case output of
                                        Just "-" -> putStrLn c
                                        Just file -> writeFile file c
                                        Nothing -> putStrLn c
                                Bytecode -> do
                                    let bytecode = toBytecode expr
                                    case output of
                                        Just "-" -> B.hPut stdout bytecode
                                        Just file -> B.writeFile file bytecode
                                        Nothing -> B.hPut stdout bytecode
