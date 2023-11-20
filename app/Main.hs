{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import BytecodeCompiler qualified
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Identity (Identity (..))
import Control.Monad.State (StateT (runStateT), evalStateT)
import Control.Monad.State.Lazy
import Data.ByteString.Lazy qualified as B
import Data.List (intercalate)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Void
import Data.Void qualified
import GHC.IO.Handle (hFlush)
import GHC.IO.StdHandles (stdout)
import Options.Applicative
import Parser
import System.Exit (exitFailure, exitSuccess)
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)
import System.TimeIt
import Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)
import VM qualified
import Verifier

data Options = Options
    { input :: Maybe FilePath
    , output :: Maybe FilePath
    , debug :: Bool
    , verbose :: Bool
    , emitBytecode :: Bool
    , runBytecode :: Bool
    , breakpoints :: Maybe String
    , showTime :: Bool
    , runRepl :: Bool
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
        <*> switch (long "profile" <> short 'p' <> help "Show time spent parsing, compiling and running")
        <*> switch (long "repl" <> short 'e' <> help "Start REPL")

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

potentiallyTimedOperation :: (MonadIO m) => String -> Bool -> m a -> m a
potentiallyTimedOperation msg showTime action = do
    if showTime then timeItNamed msg action else action

parseAndVerify :: String -> T.Text -> CompilerFlags -> IO (Either (ParseErrorBundle T.Text Void) Program)
parseAndVerify name t cf = do
    let (result, state) = runIdentity $ runStateT (parseProgram' t) (ParserState{compilerFlags = cf, validLets = [], validFunctions = []})
    case result of
        Left err -> return $ Left err
        Right program' -> do
            let (Program exprs) = program'
            verifierResult <- verifyProgram name t exprs
            case verifierResult of
                Left err -> return $ Left err
                Right _ -> return $ Right program'

-- parse :: String -> String -> CompilerFlags -> IO (Either (ParseErrorBundle T.Text Data.Void.Void) (Program))
parse name input compilerFlags = do
    return $ parseAndVerify name (T.pack input) CompilerFlags{verboseMode = False}

main :: IO ()
main = do
    Options input output debug verbose emitBytecode runBytecode breakpoints showTime runRepl <-
        execParser $
            info
                (optionsParser <**> helper)
                ( fullDesc
                    <> progDesc "TODO"
                    <> header "indigo - a functional programming language"
                )
    when runRepl $ do
        runStateT repl initREPLState
        exitSuccess
    program <-
        if not runBytecode
            then do
                i <- inputFile input
                -- prog <- liftIO $ potentiallyTimedOperation "Parsing" showTime (parse (fromJust input) i CompilerFlags{verboseMode = verbose})
                -- let expr = case prog of
                --         Left err -> error $ "Parse error: " ++ errorBundlePretty err
                --         Right expr -> expr
                prog <- parse (fromJust input) i CompilerFlags{verboseMode = verbose}
                rprog <- prog
                expr <- case rprog of
                    Left err -> error $ "Parse error: " ++ errorBundlePretty err
                    Right expr -> return expr

                when debug $ putStrLn $ prettyPrintProgram expr
                potentiallyTimedOperation "Compilation" showTime (evalStateT (BytecodeCompiler.compileProgram expr) (BytecodeCompiler.initCompilerState expr))
            else do
                bytecode <- inputFileBinary input
                return $ VM.fromBytecode bytecode
    when emitBytecode $ do
        case output of
            Just "-" -> B.putStr $ VM.toBytecode $ V.fromList program
            Just file -> B.writeFile file $ VM.toBytecode $ V.fromList program
            Nothing -> error "No output file specified"
        exitSuccess

    when debug $ putStrLn $ VM.printAssembly (V.fromList program) False

    let mainPc = BytecodeCompiler.locateLabel program "main"
    let breakpoints' = fromMaybe [] $ breakpoints >>= \x -> return $ map read $ words x :: Maybe [Int]
    potentiallyTimedOperation "VM" showTime $ VM.runVM $ (VM.initVM (V.fromList program)){VM.pc = mainPc, VM.breakpoints = breakpoints', VM.callStack = [VM.StackFrame{returnAddress = mainPc, locals = []}]}

-- REPL
data REPLState = REPLState
    { program :: Program
    , previousProgram :: Program
    , previousStack :: [VM.Data]
    }

initREPLState =
    REPLState
        { -- program = Program [FuncDef "main" [] (DoBlock [])],
          program = Program []
        , previousProgram = Program []
        , previousStack = []
        }

repl :: StateT REPLState IO ()
repl = do
    liftIO $ putStr "🔮> "
    liftIO $ hFlush stdout
    input <- liftIO getLine
    case input of
        ":exit" -> return ()
        ":ast" -> do
            Program sexprs <- gets program
            liftIO $ putStrLn $ prettyPrintProgram (Program sexprs)
            repl
        ":vm" -> do
            Program sexprs <- gets program
            compiled <- liftIO $ evalStateT (BytecodeCompiler.compileProgram (Program sexprs)) (BytecodeCompiler.initCompilerState (Program sexprs))
            liftIO $ putStrLn $ VM.printAssembly (V.fromList compiled) True
            repl
        ":help" -> do
            liftIO $ putStrLn "Commands:"
            liftIO $ putStrLn ":exit - exit the REPL"
            liftIO $ putStrLn ":ast - print the AST"
            liftIO $ putStrLn ":vm - print the VM assembly"
            repl
        _ -> do
            let (result, state) = runIdentity $ runStateT (parseProgram' (T.pack input)) (ParserState{compilerFlags = CompilerFlags{verboseMode = False}, validLets = [], validFunctions = []})
            case result of
                Left err -> do
                    liftIO $ putStrLn $ errorBundlePretty err
                    repl
                Right program' -> do
                    let (Program pexrs) = program'
                    (Program sexprs) <- gets program
                    modify (\s -> s{previousProgram = Program sexprs})
                    modify (\s -> s{program = Program (sexprs ++ pexrs)})
                    mergedProgram <- gets program >>= \x -> return $ Program (exprs x ++ [FuncDef "main" [] (DoBlock [])])
                    compiled <- liftIO $ evalStateT (BytecodeCompiler.compileProgram mergedProgram) (BytecodeCompiler.initCompilerState mergedProgram)
                    let mainPc = BytecodeCompiler.locateLabel compiled "main"
                    result <- liftIO $ try (VM.runVMVM $ (VM.initVM (V.fromList compiled)){VM.pc = mainPc, VM.callStack = [VM.StackFrame{returnAddress = mainPc, locals = []}]}) :: StateT REPLState IO (Either SomeException VM.VM)
                    case result of
                        Left err -> do
                            liftIO $ putStrLn $ VM.printAssembly (V.fromList compiled) True
                            liftIO $ print err
                            previousProgram <- gets previousProgram
                            modify (\s -> s{program = previousProgram})
                            repl
                        Right vm -> do
                            previousStack <- gets previousStack
                            when (previousStack /= VM.stack vm) $ do
                                liftIO $ print $ head $ VM.stack vm
                            modify (\s -> s{previousStack = VM.stack vm})
                            repl
