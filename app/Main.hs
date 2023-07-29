module Main (main, inputFile) where

import BytecodeCompiler qualified
import Control.Monad
import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.State (evalStateT)
import Data.ByteString.Lazy qualified as B
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import GHC.IO.Handle (hFlush)
import GHC.IO.StdHandles (stdout)
import Options.Applicative
import Parser
    ( CompilerFlags (CompilerFlags, verboseMode)
    , Expr (DoBlock, FuncDef)
    , Program (..)
    , parseProgram
    )
import System.Exit (exitSuccess)
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)
import Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)
import VM qualified

data Options = Options
    { input :: Maybe FilePath
    , output :: Maybe FilePath
    , debug :: Bool
    , verbose :: Bool
    , emitBytecode :: Bool
    , runBytecode :: Bool
    , breakpoints :: Maybe String
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
                    <> help "Output file for bytecode compilation. Use - for stdout."
                )
            )
        <*> switch
            ( long "debug"
                <> short 'd'
                <> help "Enable debug mode"
            )
        <*> switch (long "verbose" <> short 'v' <> help "Enable verbose mode")
        <*> switch (long "emit-bytecode" <> short 'b' <> help "Emit bytecode")
        <*> switch (long "run-bytecode" <> short 'r' <> help "Run bytecode")
        <*> optional (strOption (long "breakpoints" <> short 't' <> help "Breakpoints for the VM to trace (space separated list of program counter indices). -1 to trace on every instruction"))

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
    Options input output debug verbose emitBytecode runBytecode breakpoints <-
        execParser $
            info
                (optionsParser <**> helper)
                ( fullDesc
                    <> progDesc "TODO"
                    <> header "prisma - a functional programming language"
                )
    program <-
        if not runBytecode
            then do
                i <- inputFile input
                let expr = case parseProgram (T.pack i) CompilerFlags{verboseMode = verbose} of
                        Left err -> error $ "Parse error: " ++ errorBundlePretty err
                        Right expr -> expr
                when debug $ putStrLn $ prettyPrintProgram expr
                evalStateT (BytecodeCompiler.compileProgram expr) (BytecodeCompiler.CompilerState expr [] [] 0)
            else do
                bytecode <- inputFileBinary input
                return $ VM.fromBytecode bytecode
    when emitBytecode $ do
        case output of
            Just "-" -> B.putStr $ VM.toBytecode program
            Just file -> B.writeFile file $ VM.toBytecode program
            Nothing -> error "No output file specified"
        exitSuccess

    when debug $ putStrLn $ VM.printAssembly program True

    let mainPc = BytecodeCompiler.locateLabel program "main"
    let breakpoints' = fromMaybe [] $ breakpoints >>= \x -> return $ map read $ words x :: Maybe [Int]
    VM.runVM $ (VM.initVM program){VM.pc = mainPc, VM.breakpoints = breakpoints', VM.callStack = [VM.StackFrame{returnAddress = mainPc, locals = []}]}