module IntegrationSpec (spec) where

import BytecodeCompiler
    ( compileProgram
    , initCompilerState
    , locateLabel
    )
import Control.Monad.State (evalStateT)
import Data.Text qualified
import Data.Vector qualified as V
import Parser
    ( CompilerFlags (CompilerFlags, verboseMode)
    , parseProgram
    )
import Parser qualified
import Test.Hspec (describe, it, shouldReturn, xit)
import Test.QuickCheck (Testable (property))
import Text.Megaparsec (errorBundlePretty)
import Text.RawString.QQ (r)
import VM
    ( IOBuffer (output)
    , IOMode (VMBuffer)
    , StackFrame (StackFrame, locals, returnAddress)
    , VM (breakpoints, callStack, ioBuffer, ioMode, pc)
    , initVM
    , runVMVM
    )

compileAndRun :: String -> IO String
compileAndRun prog = do
    let p = parseProgram (Data.Text.pack prog) Parser.initCompilerFlags
    case p of
        Left err -> error $ errorBundlePretty err
        Right program -> do
            xxx <- evalStateT (compileProgram program) (initCompilerState program)
            let xxxPoint = locateLabel xxx "main"
            -- putStrLn $ printAssembly (V.fromList xxx) True
            vm <- runVMVM $ (initVM (V.fromList xxx)){pc = xxxPoint, breakpoints = [], callStack = [StackFrame{returnAddress = xxxPoint, locals = []}], ioMode = VMBuffer}
            pure $ output $ ioBuffer vm

spec = do
    describe "Hello World" $ do
        it "Should print Hello, world!" $ do
            compileAndRun "let main => IO = println \"Hello, world!\"" `shouldReturn` "Hello, world!\n"
    describe "println" $ do
        it "Can print any string" $ do
            property $ \s -> compileAndRun ("let main => IO = println " ++ show s) `shouldReturn` (s ++ "\n")
    describe "Operator precedence" $ do
        it "Has working precedence for multiplication" $ do
            compileAndRun "let main => IO = println 1 + 2 * 3" `shouldReturn` "7\n"
        it "Has working precedence for brackets" $ do
            compileAndRun "let main => IO = println (1 + 2) * 3" `shouldReturn` "9\n"
    describe "Pattern matching" $ do
        it "Can return the first element of a list" $ do
            compileAndRun "t :: List{Int} -> Int\nt (x:xs) = x\nlet main => IO = println (t [1, 2, 3])" `shouldReturn` "1\n"
        it "Can return the first two" $ do
            compileAndRun "t :: List{Int} -> Int\nt (x:y:xs) = x + y\nlet main => IO = println (t [1, 2, 3])" `shouldReturn` "3\n"
        it "Can return the excess" $ do
            compileAndRun "t :: List{Int} -> Int\nt (x:y:xs) = xs\nlet main => IO = println (t [1, 2, 3])" `shouldReturn` "[3]\n"
        it "Can detect zero elements" $ do
            compileAndRun
                [r|t :: List{Int} -> Int
                            t [] = 0
                            t (x:xs) = x
                            let main => IO = println (t [])|]
                `shouldReturn` "0\n"
        it "Can detect one element" $ do
            compileAndRun
                [r|t :: List{Int} -> Int
                            t [] = 0
                            t (x:[]) = 1
                            t (x:xs) = x
                            let main => IO = println (t [4])|]
                `shouldReturn` "1\n"
    describe "Prelude" $ do
        it "Can use map" $ do
            compileAndRun "let main => IO = println (map (`+`1), [1, 2, 3])" `shouldReturn` "[2,3,4]\n"
        it "Can use sum on integers" $ do
            compileAndRun "let main => IO = println (sum [1, 2, 3])" `shouldReturn` "6\n"
        xit "Can use sum on floats" $ do
            compileAndRun "let main => IO = println (sum [1.0, 2.0, 3.0])" `shouldReturn` "6.0\n"
        xit "Can use foldl" $ do
            compileAndRun "let main => IO = println (foldl (`+`), 0, [1, 2, 3])" `shouldReturn` "6\n"
    describe "Implicit casting" $ do
        it "Can cast from int to float" $ do
            compileAndRun [r|let main => IO = println ^2 + 4.0|] `shouldReturn` "6.0\n"
            compileAndRun [r|let main => IO = println 2.0 + ^4|] `shouldReturn` "6.0\n"
        it "Can cast from float to int" $ do
            compileAndRun [r|let main => IO = println ^2.0 + 4|] `shouldReturn` "6\n"
            compileAndRun [r|let main => IO = println 2 + ^4.0|] `shouldReturn` "6\n"
        it "Can cast from int to string" $ do
            compileAndRun [r|let main => IO = println ^2 + "test"|] `shouldReturn` "2test\n"
        it "Can cast from string to int" $ do
            compileAndRun [r|let main => IO = println ^"2" + 4|] `shouldReturn` "6\n"
    describe "Explicit casting" $ do
        it "Can cast from int to float" $ do
            compileAndRun [r|let main => IO = println (2 as Float) + 4.0|] `shouldReturn` "6.0\n"
            compileAndRun [r|let main => IO = println 2.0 + (4 as Float)|] `shouldReturn` "6.0\n"
        it "Can cast a function call" $ do
            compileAndRun
                [r|f :: Int -> Int
                            f x = x + 1
                            let main => IO = println ((f 2) as Float) + 4.0|]
                `shouldReturn` "7.0\n"
    describe "Overloading" $ do
        it "Can find function based on type" $ do
            compileAndRun
                [r|
                f :: Int -> Int
                f x = x + 1
                f :: String -> String
                f x = "'":x:"'" 
                let main => IO = do
                    println f 1
                    println f "test"
                end|]
                `shouldReturn` "2\n'test'\n"
        it "Can find function based on type with lists" $ do
            compileAndRun
                [r|
                f :: List{Int} -> Int
                f x = sum x
                f :: List{String} -> String
                f x = x
                let main => IO = do
                    println f [1, 2, 3]
                    println f ["test", "test2"]
                end|]
                `shouldReturn` "6\n[test,test2]\n"
    describe "Structs" $ do
        it "Can evaluate the point example" $ do
            compileAndRun
                [r|
                struct Point = (x: Int, y: Int)

                let main => IO = do
                    let p = Point { x : 1, y : 2 }
                    println p.x
                    println p.y
                end|]
                `shouldReturn` "1\n2\n"
        it "Can access fields" $ do
            compileAndRun
                [r|
                struct Dog = (name: String)
                struct Cat = (name: String)

                let main => IO = do
                    let bello = Dog { name : "Bello" }
                    let mauzi = Cat { name : "Mauzi" }
                    println bello.name
                    println mauzi.name
                end|]
                `shouldReturn` "Bello\nMauzi\n"
        it "Can be passed via functions" $ do
            compileAndRun
                [r|
                struct Dog = (name: String)
                struct Cat = (name: String)

                getName :: Dog -> String
                getName self = self.name

                getName :: Cat -> String
                getName self = self.name

                let main => IO = do
                    let bello = Dog { name : "Bello" }
                    let mauzi = Cat { name : "Mauzi" }
                    println (getName bello)
                    println (getName mauzi)
                end|]
                `shouldReturn` "Bello\nMauzi\n"
        it "Can access fields via automatically generated functions" $ do
            compileAndRun
                [r|
                struct Dog = (name: String)
                struct Cat = (name: String)

                let main => IO = do
                    let bello = Dog { name : "Bello" }
                    let mauzi = Cat { name : "Mauzi" }
                    println name bello
                    println name mauzi
                end|]
                `shouldReturn` "Bello\nMauzi\n"
    describe "Traits" $ do
        it "Can use a trait" $ do
            compileAndRun
                [r|
                    trait Animal = do
                        makeNoise :: Self -> IO
                    end

                    impl Animal for Dog = do
                        makeNoise self = println "Woof"
                    end

                    impl Animal for Cat = do
                        makeNoise self = println "Meow"
                    end

                    let main => IO = do
                        makeNoise (Dog {})
                        makeNoise (Cat {})
                    end
                |]
                `shouldReturn` "Woof\nMeow\n"
    describe "Lambdas" $ do
        it "Can use a lambda in the map function" $ do
            compileAndRun
                [r|
                    let main => IO = do
                        println map (\x -> x + 1), [1, 2, 3]
                    end
                |]
                `shouldReturn` "[2,3,4]\n"
        it "Can use a lambda calling a nested function" $ do
            compileAndRun
                [r|
                    let main => IO = do
                        let square (x: Int) = x * x
                        let toThird (x: Int) = x * square x
                        println map (\x -> toThird x), [1, 2, 3]
                    end
                |]
                `shouldReturn` "[1,8,27]\n"
        it "Can use strict values in lambda" $ do
            compileAndRun
                [r|
                    let main => IO = do
                        let strict = $2
                        println map (\x -> x * strict), [1, 2, 3]
                    end
                |]
                `shouldReturn` "[2,4,6]\n"
    describe "Recursion" $ do
        it "Can use recursion" $ do
            compileAndRun
                [r|
                let rec (x: Int) => Int = do
                  if x == 0 then do
                    0
                  else do
                    rec ((x) - 1)
                  end
                end

                let main => IO = do
                    println (rec 10)
                end
                |]
                `shouldReturn` "0\n"
    describe "FFI" $ do
        it "Hello, World!" $ do
            compileAndRun
                [r|
                external "__default" = do
                    puts :: String -> IO
                end
                
                let main => IO = do
                    puts "Hello, World!\n"
                end
                |]
                `shouldReturn` ""
        it "strlen" $ do
            compileAndRun
                [r|
                external "__default" = do
                    strlen :: String -> Int
                end
                
                let main => IO = do
                    println strlen "Hello, World!\n"
                end
                |]
                `shouldReturn` "14\n"

    describe "No main" $ do
        it "Hello World" $ do
            compileAndRun
                [r|
                    println "Hello, World!"
                |]
                `shouldReturn` "Hello, World!\n"
        it "\"99\" bottles of beer" $ do
            compileAndRun
                [r|
                    let bottles (i: Int) => IO = do
                        if i > 0 then do
                            println ^i : " bottles of beer on the wall, " : ^i : " bottles of beer."
                            println "Take one down and pass it around, " : ((i) - 1) as String : " bottles of beer on the wall.\n"
                            bottles (i)-1
                        else do
                            println "No more bottles of beer on the wall, no more bottles of beer."
                            println "Go to the store and buy some more, 99 bottles of beer on the wall."
                        end
                    end

                    bottles 3
                |]
                `shouldReturn` "3 bottles of beer on the wall, 3 bottles of beer.\nTake one down and pass it around, 2 bottles of beer on the wall.\n\n2 bottles of beer on the wall, 2 bottles of beer.\nTake one down and pass it around, 1 bottles of beer on the wall.\n\n1 bottles of beer on the wall, 1 bottles of beer.\nTake one down and pass it around, 0 bottles of beer on the wall.\n\nNo more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall.\n"

-- describe "Import"
-- it "Can find function based on type with lists, multiple definitions and pattern matching" $ do
--     compileAndRun
--         [r|
--         f :: List{Int} -> Int
--         f [] = 0
--         f (x:xs) = x + (f xs)
--         f :: List{Float} -> Float
--         # f [] = 0.0
--         f (x:xs) = x + ((f xs) as Float)
--         # f :: List{String} -> String
--         # f x = x
--         let main => IO = do
--             println f [1, 2, 3]
--             println f [1.0, 2.0, 3.0]
--             # println f ["test", "test2"]
--         end|]
--         `shouldReturn` "6\n6.0\ntesttest2\n"
