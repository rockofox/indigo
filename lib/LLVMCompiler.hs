{-# LANGUAGE OverloadedStrings #-}

module LLVMCompiler
    ( compileToLLVM
    , compileToModule
    , compileProgramToLLVM
    , compileToNative
    , compileProgramToNative
    , findClang
    )
where

import BytecodeCompiler qualified
import Control.Exception (SomeException, try)
import Control.Monad (filterM)
import Control.Monad.State (evalStateT)
import Data.List (isInfixOf)
import LLVM.Compiler (compileInstructionsToLLVM, compileInstructionsToModule)
import LLVM.Types (LLVMModule)
import Parser qualified
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , findExecutable
    , getTemporaryDirectory
    , listDirectory
    )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import VM (Instruction)
import X86.Runtime (cRuntimeSource)

compileToLLVM :: [Instruction] -> String
compileToLLVM = compileInstructionsToLLVM

compileToModule :: [Instruction] -> LLVMModule
compileToModule = compileInstructionsToModule

compileProgramToLLVM :: Parser.Program -> IO (Either String String)
compileProgramToLLVM prog = do
    res <- evalStateT (BytecodeCompiler.compileProgram prog) (BytecodeCompiler.initCompilerState prog)
    case res of
        Left instrs -> return $ Right $ compileToLLVM instrs
        Right errs -> return $ Left $ "Compilation failed with " ++ show (length errs) ++ " errors"

findClang :: IO (Either String FilePath)
findClang = do
    envClang <- lookupEnv "INDIGO_CLANG"
    case envClang of
        Just p | not (null p) -> return $ Right p
        _ -> do
            mPath <- findExecutable "clang"
            case mPath of
                Just p -> do
                    testRes <- try (readProcessWithExitCode p ["-dumpmachine"] "") :: IO (Either SomeException (ExitCode, String, String))
                    case testRes of
                        Right (ExitSuccess, out, _) | "wasi" `isInfixOf` out -> checkNixClang p
                        Right (ExitSuccess, _, _) -> return $ Right p
                        _ -> checkNixClang p
                Nothing -> checkNixClang "clang"
  where
    checkNixClang fallback = do
        nixClangs <- listDirectoryNixClang
        case nixClangs of
            (c : _) -> return $ Right c
            [] -> return $ Right fallback

    listDirectoryNixClang = do
        exists <- doesDirectoryExist "/nix/store"
        if not exists
            then return []
            else do
                entries <- listDirectory "/nix/store"
                let candidates = ["/nix/store" </> e </> "bin" </> "clang" | e <- entries, "clang-wrapper" `isInfixOf` e]
                filterM doesFileExist candidates

compileToNative :: [Instruction] -> FilePath -> IO (Either String FilePath)
compileToNative instrs outPath = do
    clangRes <- findClang
    case clangRes of
        Left err -> return $ Left err
        Right clangPath -> do
            tempDir <- getTemporaryDirectory
            let workDir = tempDir </> "indigo_llvm_build"
            createDirectoryIfMissing True workDir
            let llPath = workDir </> "output.ll"
            let runtimePath = workDir </> "indigo_runtime.c"
            writeFile llPath (compileToLLVM instrs)
            writeFile runtimePath cRuntimeSource
            res <-
                try
                    ( readProcessWithExitCode
                        clangPath
                        ["-O2", "-Wno-unused-command-line-argument", llPath, runtimePath, "-lm", "-o", outPath]
                        ""
                    )
                    :: IO (Either SomeException (ExitCode, String, String))
            case res of
                Left err -> return $ Left $ "Failed to invoke clang: " ++ show err
                Right (ExitSuccess, _, _) -> return $ Right outPath
                Right (ExitFailure code, stdoutMsg, stderrMsg) ->
                    return $
                        Left $
                            "clang compilation failed (exit code "
                                ++ show code
                                ++ "):\n"
                                ++ stderrMsg
                                ++ "\n"
                                ++ stdoutMsg

compileProgramToNative :: Parser.Program -> FilePath -> IO (Either String FilePath)
compileProgramToNative prog outPath = do
    res <- evalStateT (BytecodeCompiler.compileProgram prog) (BytecodeCompiler.initCompilerState prog)
    case res of
        Left instrs -> compileToNative instrs outPath
        Right errs -> return $ Left $ "Compilation failed with " ++ show (length errs) ++ " errors"
