{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module X86CompilerSpec (spec) where

import BytecodeCompiler (compileProgram, initCompilerState)
import Control.Monad.State (evalStateT)
import Data.Text qualified as T
import Parser (initCompilerFlags, parseProgram)
import System.Directory (removeFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcess)
import Test.Hspec
import Text.RawString.QQ (r)
import VM (Action (..), Data (..), Instruction (..))
import X86Compiler

compileNativeAndRun :: String -> IO String
compileNativeAndRun code = withSystemTempDirectory "x86-test" $ \dir -> do
    let binPath = dir ++ "/test_bin"
    case parseProgram (T.pack code) initCompilerFlags of
        Left err -> error (show err)
        Right prog -> do
            compileRes <- evalStateT (compileProgram prog) (initCompilerState prog)
            case compileRes of
                Right errs -> error (show errs)
                Left instrs -> do
                    compileToNative instrs binPath
                    readProcess binPath [] ""

spec :: Spec
spec = describe "X86Compiler" $ do
    describe "Assembly Emission" $ do
        it "emits x86 assembly header and text section" $ do
            let asm = compileToX86 [Push (DInt 42), Builtin Print]
            asm `shouldSatisfy` (\s -> ".intel_syntax noprefix" `T.isInfixOf` T.pack s)
            asm `shouldSatisfy` (\s -> ".globl main" `T.isInfixOf` T.pack s)

    describe "End-to-End Native Compilation" $ do
        it "compiles and runs simple integer addition" $ do
            let code =
                    [r|
let main : IO<Unit> = do
    println (40 + 2)
end
|]
            out <- compileNativeAndRun code
            out `shouldBe` "42\n"

        it "compiles and runs recursive fibonacci" $ do
            let code =
                    [r|
let fib (0: Int) : Int = 0
let fib (1: Int) : Int = 1
let fib (n: Int) : Int = (fib ((n) - 1)) + (fib ((n) - 2))

let main : IO<Unit> = do
    println (fib 10)
end
|]
            out <- compileNativeAndRun code
            out `shouldBe` "55\n"

        it "compiles and runs string concatenation and printing" $ do
            let code =
                    [r|
let main : IO<Unit> = do
    println ("Hello, " ++ "World!")
end
|]
            out <- compileNativeAndRun code
            out `shouldBe` "Hello, World!\n"

        it "compiles and runs structs and field access" $ do
            let code =
                    [r|
type Point = (x: Int, y: Int)

let main : IO<Unit> = do
    let p = Point{x: 10, y: 20}
    println (p.x + p.y)
end
|]
            out <- compileNativeAndRun code
            out `shouldBe` "30\n"

        it "compiles and runs generic struct with trait constraints" $ do
            let code =
                    [r|
trait Number
impl Number for Int
impl Number for Float

type Example<N: Number> = (content: N)

let main: IO<Unit> = do
    let x = Example<Int>{content: 42}
    let y = Example<Float>{content: 3.4}
    println x.content
    println y.content
end
|]
            out <- compileNativeAndRun code
            out `shouldBe` "42\n3.4\n"

        it "compiles and runs pattern matching with when" $ do
            let code =
                    [r|
let main : IO<Unit> = do
    let x = 2
    when x of
        1 -> println "one"
        2 -> println "two"
        else -> println "other"
    end
end
|]
            out <- compileNativeAndRun code
            out `shouldBe` "two\n"
