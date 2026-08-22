{-# LANGUAGE OverloadedStrings #-}

module X86Compiler
    ( compileToX86
    , compileToAssemblyFile
    , compileProgramToX86
    , compileToNative
    , compileProgramToNative
    , cRuntimeSource
    )
where

import BytecodeCompiler qualified
import Control.Exception (SomeException, try)
import Control.Monad.State (evalStateT)
import Parser qualified
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import VM (Instruction)
import X86.Compiler (compileInstructionsToAsm, compileProgramToAssemblyFile)
import X86.Emitter (emitAssembly)
import X86.Runtime (cRuntimeSource)
import X86.Types (AssemblyFile)

compileToX86 :: [Instruction] -> String
compileToX86 instrs = emitAssembly (compileInstructionsToAsm instrs)

compileToAssemblyFile :: [Instruction] -> AssemblyFile
compileToAssemblyFile = compileProgramToAssemblyFile

compileProgramToX86 :: Parser.Program -> IO (Either String String)
compileProgramToX86 prog = do
    res <- evalStateT (BytecodeCompiler.compileProgram prog) (BytecodeCompiler.initCompilerState prog)
    case res of
        Left instrs -> return $ Right $ compileToX86 instrs
        Right errs -> return $ Left $ "Compilation failed with " ++ show (length errs) ++ " errors"

compileToNative :: [Instruction] -> FilePath -> IO (Either String FilePath)
compileToNative instrs outPath = do
    tempDir <- getTemporaryDirectory
    let workDir = tempDir </> "indigo_x86_build"
    createDirectoryIfMissing True workDir
    let asmPath = workDir </> "output.s"
    let runtimePath = workDir </> "indigo_runtime.c"
    writeFile asmPath (compileToX86 instrs)
    writeFile runtimePath cRuntimeSource
    res <- try (readProcessWithExitCode "gcc" ["-O2", asmPath, runtimePath, "-lm", "-o", outPath] "") :: IO (Either SomeException (ExitCode, String, String))
    case res of
        Left err -> return $ Left $ "Failed to invoke gcc: " ++ show err
        Right (ExitSuccess, _, _) -> return $ Right outPath
        Right (ExitFailure code, stdoutMsg, stderrMsg) ->
            return $ Left $ "gcc compilation failed (exit code " ++ show code ++ "):\n" ++ stderrMsg ++ "\n" ++ stdoutMsg

compileProgramToNative :: Parser.Program -> FilePath -> IO (Either String FilePath)
compileProgramToNative prog outPath = do
    res <- evalStateT (BytecodeCompiler.compileProgram prog) (BytecodeCompiler.initCompilerState prog)
    case res of
        Left instrs -> compileToNative instrs outPath
        Right errs -> return $ Left $ "Compilation failed with " ++ show (length errs) ++ " errors"
