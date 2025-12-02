{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import AST qualified
import BytecodeCompiler (CompilerError (..), compileFail, renderCompilerErrors)
import BytecodeCompiler qualified
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Identity (Identity (..))
import Control.Monad.State (StateT (runStateT), evalStateT)
import Control.Monad.State.Lazy
import Data.Bool (bool)
import Data.ByteString.Lazy qualified as B
import Data.List (intercalate, isPrefixOf)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Monoid
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Void
import Data.Void qualified
import Debug.Trace
import ErrorRenderer (parseErrorBundleToSourceErrors, renderErrors)
import GHC.IO.Handle (hFlush)
import GHC.IO.StdHandles (stdout)
import Optimizer
import Options.Applicative
import Parser
import System.Console.Repline
import System.Exit (exitFailure, exitSuccess)
import System.FilePath
import System.TimeIt
import Text.Megaparsec.Error (ParseErrorBundle)
import VM qualified

data CmdOptions = Options
    { inputs :: [FilePath]
    , output :: Maybe FilePath
    , debugAST :: Bool
    , debugBytecode :: Bool
    , verbose :: Bool
    , emitBytecode :: Bool
    , runBytecode :: Bool
    , breakpoints :: Maybe String
    , showTime :: Bool
    , showVersion :: Bool
    , noVerify :: Bool
    , noOptimize :: Bool
    }

optionsParser :: Options.Applicative.Parser CmdOptions
optionsParser =
    Options
        <$> many (argument str (metavar "FILE"))
        <*> optional
            ( strOption
                ( long "output"
                    <> short 'o'
                    <> metavar "FILE"
                    <> help "Output file for bytecode compilation. Use - for stdout."
                )
            )
        <*> switch
            ( long "debug-ast"
                <> help "Output AST for debugging"
            )
        <*> switch
            ( long "debug-bytecode"
                <> short 'd'
                <> help "Output bytecode for debugging"
            )
        <*> switch (long "verbose" <> short 'v' <> help "Enable verbose mode (deprecated/broken)")
        <*> switch (long "emit-bytecode" <> short 'b' <> help "Emit bytecode")
        <*> switch (long "run-bytecode" <> short 'r' <> help "Run bytecode")
        <*> optional (strOption (long "breakpoints" <> short 't' <> help "Breakpoints for the VM to trace (space separated list of program counter indices). -1 to trace on every instruction"))
        <*> switch (long "profile" <> short 'p' <> help "Show time spent parsing, compiling and running")
        <*> switch (long "version" <> short 'V' <> help "Show version")
        <*> switch (long "no-verify" <> short 'n' <> help "Don't verify the program")
        <*> switch (long "no-optimize" <> short 'O' <> help "Don't optimize the program")

prettyPrintExpr :: Expr -> Int -> String
prettyPrintExpr (DoBlock exprs _) i = indent i ++ "DoBlock[\n" ++ intercalate "\n" (map (\x -> prettyPrintExpr x (i + 1)) exprs) ++ "\n" ++ indent i ++ "]"
prettyPrintExpr (FuncDef name _ expr _) i =
    case expr of
        DoBlock _ _ -> "FuncDef[<" ++ name ++ ">\n" ++ prettyPrintExpr expr (i + 1) ++ "\n" ++ indent i ++ "]"
        _ -> "FuncDef[<" ++ name ++ ">" ++ prettyPrintExpr expr 0 ++ "]"
prettyPrintExpr x i = indent i ++ show x

indent :: Int -> String
indent i = replicate (i * 2) ' '

prettyPrintProgram :: Program -> String
prettyPrintProgram (Program exprs _) = intercalate "\n" (map (`prettyPrintExpr` 0) exprs)

inputFile :: Maybe String -> IO String
inputFile input = case input of
    Just "-" -> getContents
    Just file -> readFile file
    Nothing -> errorWithoutStackTrace "No input file specified"

inputFileBinary :: Maybe String -> IO B.ByteString
inputFileBinary input = case input of
    Just "-" -> B.getContents
    Just file -> B.readFile file
    Nothing -> errorWithoutStackTrace "No input file specified"

potentiallyTimedOperation :: (MonadIO m) => String -> Bool -> m a -> m a
potentiallyTimedOperation msg showTime action = if showTime then timeItNamed msg action else action

parseAndVerify name input compilerFlags path =
    return
        ( do
            let result = parseProgram (T.pack input) initCompilerFlags
            case result of
                Left err -> return $ Left (err, Nothing)
                Right program' -> do
                    let (Program exprs _) = program'
                    return $ Right program'
        )

parseNoVerify name input compilerFlags = return $ do
    let result = parseProgram (T.pack input) initCompilerFlags
    case result of
        Left err -> return $ Left (err, Nothing)
        Right program' -> return $ Right program'

main :: IO ()
main = do
    Options inputs output debugAST debugBytecode verbose emitBytecode runBytecode breakpoints showTime showVersion noVerify noOptimize <-
        execParser $
            info
                (optionsParser <**> helper)
                ( fullDesc
                    <> progDesc "Run without arguments to enter the REPL."
                    <> header "indigo - a functional programming language"
                )
    when showVersion $ do
        putStrLn $ "Indigo, version " ++ version
        exitSuccess
    when (null inputs) $ do
        newRepl
        exitSuccess
    program <-
        if not runBytecode
            then do
                if length inputs == 1
                    then do
                        let input = head inputs
                        i <- readFile input
                        prog <-
                            if noVerify
                                then parseNoVerify input i initCompilerFlags
                                else parseAndVerify input i initCompilerFlags input
                        rprog <- prog
                        expr <- case rprog of
                            Left (err, expr') -> do
                                case expr' of
                                    Just expr'' -> when debugAST $ putStrLn $ prettyPrintProgram expr''
                                    Nothing -> return ()
                                errorWithoutStackTrace $ "Parse error:\n" ++ renderErrors (parseErrorBundleToSourceErrors err (T.pack i)) i
                            Right expr' -> return expr'

                        when debugAST $ putStrLn $ prettyPrintProgram expr
                        potentiallyTimedOperation "Compilation" showTime $ do
                            result <- evalStateT (BytecodeCompiler.compileProgram expr) (BytecodeCompiler.initCompilerStateWithFile expr input)
                            case result of
                                Left instructions -> return instructions
                                Right errors -> do
                                    compileFail input errors (Map.singleton input i)
                                    exitFailure
                    else do
                        moduleMapResult <- BytecodeCompiler.buildModuleMap inputs
                        moduleMap <- case moduleMapResult of
                            Left err -> errorWithoutStackTrace $ "Failed to build module map:\n" ++ err
                            Right mm -> return mm
                        let mainFile = head inputs
                        mainContent <- readFile mainFile
                        mainProg <- case parseProgram (T.pack mainContent) initCompilerFlags of
                            Left err -> errorWithoutStackTrace $ "Parse error in " ++ mainFile ++ ":\n" ++ renderErrors (parseErrorBundleToSourceErrors err (T.pack mainContent)) mainFile
                            Right p -> return p
                        when debugAST $ putStrLn $ prettyPrintProgram mainProg
                        potentiallyTimedOperation "Compilation" showTime $ do
                            result <- evalStateT (BytecodeCompiler.compileProgram mainProg) (BytecodeCompiler.initCompilerStateWithModules moduleMap mainProg mainFile)
                            case result of
                                Left instructions -> return instructions
                                Right errors -> do
                                    fileContents <- Map.fromList <$> mapM (\(_, (_, path)) -> readFile path >>= \content -> return (path, content)) (Map.toList moduleMap)
                                    let allFileContents = Map.insert mainFile mainContent fileContents
                                    compileFail mainFile errors allFileContents
                                    exitFailure
            else do
                let input = head inputs
                bytecode <- inputFileBinary (Just input)
                return $ VM.fromBytecode bytecode
    when emitBytecode $ do
        case output of
            Just "-" -> B.putStr $ VM.toBytecode $ V.fromList program
            Just file -> B.writeFile file $ VM.toBytecode $ V.fromList program
            Nothing -> errorWithoutStackTrace "No output file specified"
        exitSuccess

    when debugBytecode $ putStrLn $ VM.printAssembly (V.fromList program) False

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
        { program = Program [] Nothing
        , previousProgram = Program [] Nothing
        , previousStack = []
        }

type Repl a = HaskelineT (StateT REPLState IO) a

cmd :: String -> Repl ()
cmd input = do
    let (result, state) = runIdentity $ runStateT (parseProgram' (T.pack input)) (ParserState{compilerFlags = initCompilerFlags, validLets = [], validFunctions = []})
    case result of
        Left err -> liftIO $ putStrLn $ renderErrors (parseErrorBundleToSourceErrors err (T.pack input)) input
        Right program' -> do
            let (Program pexrs _) = program'
            (Program sexprs _) <- gets program
            modify (\s -> s{previousProgram = Program sexprs Nothing})
            modify (\s -> s{program = Program (sexprs ++ pexrs) Nothing})
            mergedProgram <- gets program >>= \x -> return $ Program (exprs x ++ [FuncDef "main" [] (DoBlock [] AST.anyPosition) AST.anyPosition]) Nothing
            compilationResult <- liftIO $ evalStateT (BytecodeCompiler.compileProgram mergedProgram) (BytecodeCompiler.initCompilerState mergedProgram)
            case compilationResult of
                Right errors -> liftIO $ putStrLn $ "Compilation errors: " ++ show errors
                Left compiled -> do
                    let mainPc = BytecodeCompiler.locateLabel compiled "main"
                    result <- liftIO $ try (VM.runVMVM $ (VM.initVM (V.fromList compiled)){VM.pc = mainPc, VM.callStack = [VM.StackFrame{returnAddress = mainPc, locals = []}]}) :: Repl (Either SomeException VM.VM)
                    case result of
                        Left err -> do
                            liftIO $ putStrLn $ VM.printAssembly (V.fromList compiled) True
                            liftIO $ print err
                            previousProgram <- gets previousProgram
                            modify (\s -> s{program = previousProgram})
                        Right vm -> do
                            previousStack <- gets previousStack
                            when (previousStack /= VM.stack vm) $ case VM.stack vm of
                                (x : _) -> liftIO $ print x
                                [] -> return ()
                            modify (\s -> s{previousStack = VM.stack vm})

replCompleter :: (Monad m, MonadState REPLState m) => WordCompleter m
replCompleter n = do
    (Program exprs _) <- gets program
    let names = map nameOf exprs
    return $ filter (isPrefixOf n) names
  where
    nameOf (FuncDef name _ _ _) = name
    nameOf (Function def dec _) = case def of
        (x : _) -> nameOf x
        [] -> ""
    nameOf _ = ""

opts :: [(String, String -> Repl ())]
opts =
    [ ("exit", const $ liftIO exitSuccess) -- TODO: use the same as final
    ,
        ( "help"
        , const $ do
            liftIO $ putStrLn ":: - start a multiline expression"
            liftIO $ putStrLn ":ast - print the AST"
            liftIO $ putStrLn ":vm - print the VM assembly"
            liftIO $ putStrLn ":help - print this help message"
            liftIO $ putStrLn ":exit / ^D - exit the REPL"
        )
    ,
        ( "ast"
        , const $ do
            (Program sexprs _) <- gets program
            liftIO $ putStrLn $ prettyPrintProgram (Program sexprs Nothing)
        )
    ,
        ( "vm"
        , const $ do
            (Program sexprs _) <- gets program
            compiled <- liftIO $ evalStateT (BytecodeCompiler.compileProgram (Program sexprs Nothing)) (BytecodeCompiler.initCompilerState (Program sexprs Nothing))
            case compiled of
                Right errors -> liftIO $ putStrLn $ "Compilation errors: " ++ show errors
                Left instructions -> liftIO $ putStrLn $ VM.printAssembly (V.fromList instructions) True
        )
    ]

version :: String
-- Breaks compile process
-- version = take 7 (giHash gi)
--   where
--     gi = $$tGitInfoCwd
version = "HEAD"

ini :: Repl ()
ini = liftIO $ putStrLn $ "Indigo, version " ++ version ++ ". Type :help for help."

final :: Repl ExitDecision
final = do
    liftIO $ putStrLn "Goodbye!"
    return Exit

newRepl :: IO ()
newRepl =
    flip evalStateT initREPLState $
        evalReplOpts $
            ReplOpts
                { banner = const $ pure "🔮> "
                , command = cmd
                , options = opts
                , prefix = Just ':'
                , multilineCommand = Just ":"
                , tabComplete = Word replCompleter
                , initialiser = ini
                , finaliser = final
                }
