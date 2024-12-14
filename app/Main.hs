{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

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
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Monoid
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Void
import Data.Void qualified
import GHC.IO.Handle (hFlush)
import GHC.IO.StdHandles (stdout)
import GitHash
import Optimizer
import Options.Applicative
import Parser
import System.Console.Repline
import System.Exit (exitFailure, exitSuccess)
import System.TimeIt
import Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)
import VM qualified
import Verifier

data CmdOptions = Options
    { input :: Maybe FilePath
    , output :: Maybe FilePath
    , debug :: Bool
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
        <$> optional (argument str (metavar "FILE"))
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
        <*> switch (long "verbose" <> short 'v' <> help "Enable verbose mode (deprecated/broken)")
        <*> switch (long "emit-bytecode" <> short 'b' <> help "Emit bytecode")
        <*> switch (long "run-bytecode" <> short 'r' <> help "Run bytecode")
        <*> optional (strOption (long "breakpoints" <> short 't' <> help "Breakpoints for the VM to trace (space separated list of program counter indices). -1 to trace on every instruction"))
        <*> switch (long "profile" <> short 'p' <> help "Show time spent parsing, compiling and running")
        <*> switch (long "version" <> short 'V' <> help "Show version")
        <*> switch (long "no-verify" <> short 'n' <> help "Don't verify the program")
        <*> switch (long "no-optimize" <> short 'O' <> help "Don't optimize the program")

prettyPrintExpr :: Expr -> Int -> String
prettyPrintExpr (DoBlock exprs) i = indent i ++ "DoBlock[\n" ++ intercalate "\n" (map (\x -> prettyPrintExpr x (i + 1)) exprs) ++ "\n" ++ indent i ++ "]"
prettyPrintExpr (FuncDef name _ expr) i =
    case expr of
        DoBlock _ -> "FuncDef[<" ++ name ++ ">\n" ++ prettyPrintExpr expr (i + 1) ++ "\n" ++ indent i ++ "]"
        _ -> "FuncDef[<" ++ name ++ ">" ++ prettyPrintExpr expr 0 ++ "]"
prettyPrintExpr x i = indent i ++ show x

indent :: Int -> String
indent i = replicate (i * 2) ' '

prettyPrintProgram :: Program -> String
prettyPrintProgram (Program exprs) = intercalate "\n" (map (`prettyPrintExpr` 0) exprs)

inputFile :: Maybe String -> IO String
inputFile input = case input of
    Just "-" -> getContents
    Just file -> readFile file
    Nothing -> error "No input file specified"

inputFileBinary :: Maybe String -> IO B.ByteString
inputFileBinary input = case input of
    Just "-" -> B.getContents
    Just file -> B.readFile file
    Nothing -> error "No input file specified"

potentiallyTimedOperation :: (MonadIO m) => String -> Bool -> m a -> m a
potentiallyTimedOperation msg showTime action = if showTime then timeItNamed msg action else action

parseAndVerify name input compilerFlags path =
    return
        ( do
            let result = parseProgram (T.pack input) initCompilerFlags
            case result of
                Left err -> return $ Left (err, Nothing)
                Right program' -> do
                    let (Program exprs) = program'
                    verifierResult <- verifyProgramWithPath name (T.pack input) exprs path
                    case verifierResult of
                        Left err -> return $ Left (err, Just program')
                        Right _ -> return $ Right program'
        )

parseNoVerify name input compilerFlags = return $ do
    let result = parseProgram (T.pack input) initCompilerFlags
    case result of
        Left err -> return $ Left (err, Nothing)
        Right program' -> return $ Right program'

main :: IO ()
main = do
    Options input output debug verbose emitBytecode runBytecode breakpoints showTime showVersion noVerify noOptimize <-
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
    when (isNothing input) $ do
        newRepl
        exitSuccess
    program <-
        if not runBytecode
            then do
                i <- inputFile input
                prog <-
                    if noVerify
                        then parseNoVerify (fromJust input) i initCompilerFlags
                        else parseAndVerify (fromJust input) i initCompilerFlags (fromJust input)
                rprog <- prog
                expr <- case rprog of
                    Left (err, expr) -> do
                        case expr of
                            Just expr -> do
                                when debug $ putStrLn $ prettyPrintProgram expr
                            Nothing -> return ()
                        error $ "Parse error: " ++ errorBundlePretty err
                    Right expr -> return expr

                when debug $ putStrLn $ prettyPrintProgram expr
                potentiallyTimedOperation "Compilation" showTime $
                    do
                        bool id optimize (not noOptimize)
                        <$> evalStateT (BytecodeCompiler.compileProgram expr) (BytecodeCompiler.initCompilerStateWithFile expr (fromJust input))
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
        { program = Program []
        , previousProgram = Program []
        , previousStack = []
        }

type Repl a = HaskelineT (StateT REPLState IO) a

cmd :: String -> Repl ()
cmd input = do
    let (result, state) = runIdentity $ runStateT (parseProgram' (T.pack input)) (ParserState{compilerFlags = initCompilerFlags, validLets = [], validFunctions = []})
    case result of
        Left err -> liftIO $ putStrLn $ errorBundlePretty err
        Right program' -> do
            let (Program pexrs) = program'
            (Program sexprs) <- gets program
            modify (\s -> s{previousProgram = Program sexprs})
            modify (\s -> s{program = Program (sexprs ++ pexrs)})
            mergedProgram <- gets program >>= \x -> return $ Program (exprs x ++ [FuncDef "main" [] (DoBlock [])])
            compiled <- liftIO $ evalStateT (BytecodeCompiler.compileProgram mergedProgram) (BytecodeCompiler.initCompilerState mergedProgram)
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
                    when (previousStack /= VM.stack vm) $ liftIO $ print $ head $ VM.stack vm
                    modify (\s -> s{previousStack = VM.stack vm})

replCompleter :: (Monad m, MonadState REPLState m) => WordCompleter m
replCompleter n = do
    (Program exprs) <- gets program
    let names = map nameOf exprs
    return $ filter (isPrefixOf n) names
  where
    nameOf (FuncDef name _ _) = name
    nameOf (Function def dec) = nameOf (head def)
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
            (Program sexprs) <- gets program
            liftIO $ putStrLn $ prettyPrintProgram (Program sexprs)
        )
    ,
        ( "vm"
        , const $ do
            (Program sexprs) <- gets program
            compiled <- liftIO $ evalStateT (BytecodeCompiler.compileProgram (Program sexprs)) (BytecodeCompiler.initCompilerState (Program sexprs))
            liftIO $ putStrLn $ VM.printAssembly (V.fromList compiled) True
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
