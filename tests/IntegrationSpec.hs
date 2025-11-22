module IntegrationSpec (spec) where

import BytecodeCompiler
    ( compileProgram
    , initCompilerState
    , locateLabel
    , renderCompilerErrors
    )
import Control.Monad.State (evalStateT)
import Data.Functor
import Data.List (elemIndex)
import Data.Text qualified
import Data.Text qualified as T
import Data.Vector qualified as V
import Optimizer
import Parser
    ( CompilerFlags (CompilerFlags, verboseMode)
    , parseProgram
    )
import Parser qualified
import Test.Hspec (describe, it, pendingWith, shouldReturn, xit)
import Test.QuickCheck (Testable (property))
import Text.Megaparsec (errorBundlePretty)
import Text.Megaparsec.Error
import Text.RawString.QQ (r)
import Util (whenErr)
import VM
    ( IOBuffer (output)
    , IOMode (VMBuffer)
    , Instruction (Label)
    , StackFrame (StackFrame, locals, returnAddress)
    , VM (breakpoints, callStack, ioBuffer, ioMode, pc)
    , initVM
    , printAssembly
    , runVMVM
    , shouldExit
    )
import Verifier

compileAndRun :: String -> IO String
compileAndRun prog = do
    let p = parseProgram (Data.Text.pack prog) Parser.initCompilerFlags
    case p of
        Left err -> error $ errorBundlePretty err
        Right program -> do
            let (Parser.Program exprs) = program
            verifierResult <- verifyProgram "test" (T.pack prog) exprs
            whenErr verifierResult $ \err -> error $ "Verifier error: " ++ errorBundlePretty err
            compilerOutput <- evalStateT (compileProgram program) (initCompilerState program)
            optimizedProgram <- case compilerOutput of
                Left bytecode -> pure (optimize bytecode)
                Right err -> error $ renderCompilerErrors err prog
            let mainLabel = locateLabel optimizedProgram "main"
            let hasTopLevelLets = any (\case Parser.Let{} -> True; _ -> False) exprs
            let startPc =
                    if hasTopLevelLets
                        then do
                            let sepIndex = elemIndex (VM.Label "__sep") optimizedProgram
                            case sepIndex of
                                Just sepPc -> sepPc + 1
                                Nothing -> 0
                        else mainLabel
            vm <- runVMVM $ (initVM (V.fromList optimizedProgram)){pc = startPc, breakpoints = [], callStack = [StackFrame{returnAddress = mainLabel, locals = []}], ioMode = VMBuffer, shouldExit = False}
            pure $ output $ ioBuffer vm

spec = do
    describe "Hello World" $ do
        it "Should print Hello, world!" $ do
            compileAndRun "let main : IO = println \"Hello, world!\"" `shouldReturn` "Hello, world!\n"
    -- describe "println" $ do
    -- it "Can print any string" $ do
    -- property $ \s -> compileAndRun ("let main : IO = println " ++ show s) `shouldReturn` (s ++ "\n")
    describe "Operator precedence" $ do
        it "Has working precedence for multiplication" $ do
            compileAndRun "let main : IO = println 1 + 2 * 3" `shouldReturn` "7\n"
        it "Has working precedence for brackets" $ do
            compileAndRun "let main : IO = println (1 + 2) * 3" `shouldReturn` "9\n"
    describe "Pattern matching" $ do
        it "Can match Int" $ do
            compileAndRun
                [r|
                t :: Int -> Int
                t 1 = 2
                t 2 = 3
                t _ = 4
                let main : IO = println (t 1)
            |]
                `shouldReturn` "2\n"
        it "Can match String" $ do
            compileAndRun
                [r|
                t :: String -> Int
                t "a" = 2
                t "b" = 3
                t _ = 4
                let main : IO = println (t "a")
            |]
                `shouldReturn` "2\n"
        it "Can match struct" $
            do
                compileAndRun
                    [r|
                bind :: Optional -> Any -> Any
                bind Some{value: x} f = f x
                bind None{} f = None{}
                bind _ = "Error: Invalid argument to bind."
                
                let main : IO = do
                    println (bind Some{value: 5}, \x -> x + 1)
                    println (bind None{}, \x -> x + 1)
                end
            |]
                `shouldReturn` "6\nNone{__traits: [Optional]}\n"
        it "Handles multiple variables correctly" $ do
            compileAndRun
                [r|
                bla :: Int -> Int -> Int
                bla 1 x = 100 + x
                bla 2 x = 200 + x
                bla 3 x = 300 + x

                let main : IO = do
                    println (bla 1, 1)
                    println (bla 2, 2)
                    println (bla 3, 3)
                end
                |]
                `shouldReturn` "101\n202\n303\n"
        it "Can match a list" $ do
            compileAndRun
                [r|
                t :: [Int] -> Int
                t [] = 0
                t (x:xs) = x
                let main : IO = println (t [1, 2, 3])
            |]
                `shouldReturn` "1\n"
        -- it "Can match last element of a list" $ do
        --     compileAndRun
        --         [r|
        --         t :: [Int] -> Int
        --         t [] = 0
        --         t [x] = x
        --         t (x:xs) = t xs
        --         let main : IO = println (t [1, 2, 3])
        --     |]
        --         `shouldReturn` "3\n"
        describe "Lists" $ do
            it "Can return the first element of a list" $ do
                compileAndRun "t :: [Int] -> Int\nt (x:xs) = x\nlet main : IO = println (t [1, 2, 3])" `shouldReturn` "1\n"
            it "Can return the first two" $ do
                compileAndRun "t :: [Int] -> Int\nt (x:y:xs) = x + y\nlet main : IO = println (t [1, 2, 3])" `shouldReturn` "3\n"
            it "Can return the excess" $ do
                compileAndRun "t :: [Int] -> Int\nt (x:y:xs) = xs\nlet main : IO = println (t [1, 2, 3])" `shouldReturn` "[3]\n"
            it "Can detect zero elements" $ do
                compileAndRun
                    [r|t :: [Int] -> Int
                                t [] = 0
                                t (x:xs) = x
                                let main : IO = println (t [] as [Int])|]
                    `shouldReturn` "0\n"
        it "Can detect one element" $ do
            compileAndRun
                [r|t :: [Int] -> Int
                            t [] = 0
                            t (x:[]) = 1
                            t (x:xs) = x
                            let main : IO = println (t [4])|]
                `shouldReturn` "1\n"
        describe "Structs" $ do
            it "Can match based on what struct" $ do
                compileAndRun
                    [r|struct Teacher = ()
                       struct Student = ()
                       struct Foo = ()

                       test :: Any -> String
                       test Teacher{} = "Teacher"
                        test Student{} = "Student"
                        test _ = "Unknown"

                        let main : IO = do
                            println test Teacher{}
                            println test Student{}
                            println test Foo{}
                        end
                    |]
                    `shouldReturn` "Teacher\nStudent\nUnknown\n"
            it "Can match struct fields" $ do
                compileAndRun
                    [r|struct Teacher = (name: String)
                        struct Student = (name: String)
                        struct Foo = (name: String)

                        test :: Any -> String
                        test Teacher{name: n} = n
                        test Student{name: n} = n
                        test _ = "Unknown"

                        let main : IO = do
                            println test Teacher{name: "Alice"}
                            println test Student{name: "Bob"}
                            println test Foo{name: "Charlie"}
                        end
                    |]
                    `shouldReturn` "Alice\nBob\nUnknown\n"
    describe "Prelude" $ do
        it "Can use map" $ do
            compileAndRun "let main : IO = println (map (`+`1), [1, 2, 3])" `shouldReturn` "[2,3,4]\n"
        it "Can use sum on integers" $ do
            pendingWith "Needs further language support"
            compileAndRun "let main : IO = println (sum [1, 2, 3])" `shouldReturn` "6\n"
        it "Can use sum on floats" $ do
            pendingWith "Needs further language support"
            compileAndRun "let main : IO = println (sum [1.0, 2.0, 3.0])" `shouldReturn` "6.0\n"
        it "Can use foldl" $ do
            compileAndRun "let main : IO = println (foldl (`+`), 0, [1, 2, 3])" `shouldReturn` "6\n"
    describe "Implicit casting" $ do
        it "Can cast from int to float" $ do
            compileAndRun [r|let main : IO = println ^2 + 4.0|] `shouldReturn` "6.0\n"
            compileAndRun [r|let main : IO = println 2.0 + ^4|] `shouldReturn` "6.0\n"
        it "Can cast from float to int" $ do
            compileAndRun [r|let main : IO = println ^2.0 + 4|] `shouldReturn` "6\n"
            compileAndRun [r|let main : IO = println 2 + ^4.0|] `shouldReturn` "6\n"
        it "Can cast from int to string" $ do
            compileAndRun [r|let main : IO = println ^2 + "test"|] `shouldReturn` "2test\n"
        it "Can cast from string to int" $ do
            compileAndRun [r|let main : IO = println ^"2" + 4|] `shouldReturn` "6\n"
    describe "Explicit casting" $ do
        it "Can cast from int to float" $ do
            compileAndRun [r|let main : IO = println (2 as Float) + 4.0|] `shouldReturn` "6.0\n"
            compileAndRun [r|let main : IO = println 2.0 + (4 as Float)|] `shouldReturn` "6.0\n"
        it "Can cast a function call" $ do
            compileAndRun
                [r|f :: Int -> Int
                            f x = x + 1
                            let main : IO = println ((f 2) as Float) + 4.0|]
                `shouldReturn` "7.0\n"
    describe "Overloading" $ do
        xit "Can find function based on type" $ do
            compileAndRun
                [r|
                f :: Int -> Int
                f x = x + 1
                f :: String -> String
                f x = "'":x:"'"
                let main : IO = do
                    println f 1
                    println f "test"
                end|]
                `shouldReturn` "2\n'test'\n"
        xit "Can find function based on type with lists" $ do
            compileAndRun
                [r|
                f :: [Int] -> Int
                f x = sum x
                f :: [String] -> String
                f x = x
                let main : IO = do
                    println f [1, 2, 3]
                    println f ["test", "test2"]
                end|]
                `shouldReturn` "6\n[test,test2]\n"
    describe "Structs" $ do
        it "Can evaluate the point example" $ do
            compileAndRun
                [r|
                struct Point = (x: Int, y: Int)

                let main : IO = do
                    let p = Point { x: 1, y: 2 }
                    println p.x
                    println p.y
                end|]
                `shouldReturn` "1\n2\n"
        it "Can access fields" $ do
            compileAndRun
                [r|
                struct Dog = (name: String)
                struct Cat = (name: String)

                let main : IO = do
                    let bello = Dog { name: "Bello" }
                    let mauzi = Cat { name: "Mauzi" }
                    println bello.name
                    println mauzi.name
                end|]
                `shouldReturn` "Bello\nMauzi\n"
        it "Can be passed via functions" $ do
            compileAndRun
                [r|
                struct Cat = (name: String)

                getName :: Cat -> String
                getName self = self.name

                let main : IO = do
                    let mauzi = Cat { name: "Mauzi" }
                    println (getName mauzi)
                end|]
                `shouldReturn` "Mauzi\n"
        xit "Can access fields via automatically generated functions" $ do
            compileAndRun
                [r|
                struct Dog = (name: String)
                struct Cat = (name: String)

                let main : IO = do
                    let bello = Dog { name: "Bello" }
                    let mauzi = Cat { name: "Mauzi" }
                    println name bello
                    println name mauzi
                end|]
                `shouldReturn` "Mauzi\n"
        it "Can use `is` syntax" $ do
            compileAndRun
                [r|
                trait Person
                trait Human
                struct Person = (name: String, age: Int) is Person, Human
                |]
                `shouldReturn` ""
    describe "Traits" $ do
        it "Can use a trait" $ do
            compileAndRun
                [r|
                    struct Dog = ()
                    struct Cat = ()

                    trait Animal = do
                        makeNoise :: Self -> IO
                    end

                    impl Animal for Dog = do
                        makeNoise self = println "Woof"
                    end

                    impl Animal for Cat = do
                        makeNoise self = println "Meow"
                    end

                    let main : IO = do
                        makeNoise (Dog {})
                        makeNoise (Cat {})
                    end
                |]
                `shouldReturn` "Woof\nMeow\n"
        it "Monad" $ do
            compileAndRun
                [r|
                    trait MMonad = do
                      mbind :: Any -> (Any -> Any) -> Any
                    end

                    struct MMaybe = (some: Any, none: Any)

                    impl MMonad for MMaybe = do
                      mbind x f = do
                        f x.some
                      end
                    end

                    let call (x: (Any -> Any)) = x "test"

                    let main : IO = do
                      let bla = MMaybe { some: "hello", none: "" }
                      mbind bla, print
                    end
                    |]
                `shouldReturn` "hello"
        it "Static dispatch" $ do
            compileAndRun
                [r|
                    printNumbersAndSum :: IO
                    printNumbersAndSum _ = (println "x") >> (return 2)

                    main :: IO
                    main _ = do
                        println printNumbersAndSum
                    end
                |]
                `shouldReturn` "x\nIO{__traits: [Monad,__field_inner], inner: 2}\n"
    describe "Lambdas" $ do
        it "Can use a lambda in the map function" $ do
            compileAndRun
                [r|
                    let main : IO = do
                        println map (\x -> x + 1), [1, 2, 3]
                    end
                |]
                `shouldReturn` "[2,3,4]\n"
        it "Can use a lambda calling a nested function" $ do
            compileAndRun
                [r|
                    let main : IO = do
                        let square (x: Int) : Int = x * x
                        let toThird (x: Int) : Int = x * square x
                        println map (\x -> toThird x as Int), [1, 2, 3]
                    end
                |]
                `shouldReturn` "[1,8,27]\n"
        it "Can use strict values in lambda" $ do
            compileAndRun
                [r|
                    let main : IO = do
                        let strict = $2
                        println map (\x -> x * strict), [1, 2, 3]
                    end
                |]
                `shouldReturn` "[2,4,6]\n"
    describe "Recursion" $ do
        it "Can use recursion" $ do
            compileAndRun
                [r|
                let rec (x: Int) : Int = do
                  if x == 0 then do
                    0
                  else do
                    rec ((x) - 1)
                  end
                end

                let main : IO = do
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

                let main : IO = do
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

                let main : IO = do
                    println strlen "Hello, World!\n"
                end
                |]
                `shouldReturn` "14\n"

    describe "Function composition" $ do
        it "Direct" $ do
            compileAndRun
                [r|
                compose :: Any -> Any -> Any
                compose f g = \x -> f (g x)

                increase :: Int -> Int
                increase x = x + 1

                let main : IO = println (compose increase, increase) 2
                |]
                `shouldReturn` "4\n"
        it "Indirect" $
            compileAndRun
                [r|
                compose :: Any -> Any -> Any
                compose f g = \x -> f (g x)

                increase :: Int -> Int
                increase x = x + 1

                increaseByTwo :: Int -> Int
                increaseByTwo x = (compose increase, increase) x

                let main : IO = println increaseByTwo 2
                |]
                `shouldReturn` "4\n"
    describe "No main" $ do
        it "Hello World" $ do
            compileAndRun
                [r|
                    let main : IO = println "Hello, World!"
                |]
                `shouldReturn` "Hello, World!\n"
        it "Functional" $ do
            compileAndRun
                [r|
                    let main : IO = print (map (`*`3), [2, 4, 6, 8])
                |]
                `shouldReturn` "[6,12,18,24]"
        it "Structured" $ do
            compileAndRun
                [r|
                    struct Person = (name: String, age: Int)
                    let peter = Person { name: "Peter", age: 24 }
                    let main : IO = println peter.name ++ " is " ++ (peter.age) as String ++ " years old"
                |]
                `shouldReturn` "Peter is 24 years old\n"
        it "Pattern matching" $ do
            compileAndRun
                [r|
                    let head ([]: [Any]) = 0
                    let head ((x:xs): [Any]) = x
                    let main : IO = print head [1,2,3]
                |]
                `shouldReturn` "1"
        it "\"99\" bottles of beer" $ do
            compileAndRun
                [r|
                    let bottles (i: Int) : IO = do
                        if i > 0 then do
                            println i as String ++ " bottles of beer on the wall, " ++ i as String ++ " bottles of beer."
                            println "Take one down and pass it around, " ++ ((i) - 1) as String ++ " bottles of beer on the wall.\n"
                            bottles (i)-1
                        else do
                            println "No more bottles of beer on the wall, no more bottles of beer."
                            println "Go to the store and buy some more, 99 bottles of beer on the wall."
                        end
                    end

                    let main : IO = bottles 3
                |]
                `shouldReturn` "3 bottles of beer on the wall, 3 bottles of beer.\nTake one down and pass it around, 2 bottles of beer on the wall.\n\n2 bottles of beer on the wall, 2 bottles of beer.\nTake one down and pass it around, 1 bottles of beer on the wall.\n\n1 bottles of beer on the wall, 1 bottles of beer.\nTake one down and pass it around, 0 bottles of beer on the wall.\n\nNo more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall.\n"
    describe "when statements" $ do
        it "Can match simple integer values" $ do
            compileAndRun
                [r|
                    let main : IO = do
                        let x = 5
                        when x of
                            1 -> println "one"
                            2 -> println "two"
                            5 -> println "five"
                            else -> println "other"
                        end
                    end
                |]
                `shouldReturn` "five\n"
        it "Can match boolean values" $ do
            compileAndRun
                [r|
                    let main : IO = do
                        let b = True
                        when b of
                            True -> println "true"
                            False -> println "false"
                            else -> println "other"
                        end
                    end
                |]
                `shouldReturn` "true\n"
        it "Can match string values" $ do
            compileAndRun
                [r|
                    let main : IO = do
                        let s = "hello"
                        when s of
                            "hello" -> println "matched hello"
                            "world" -> println "matched world"
                            else -> println "other"
                        end
                    end
                |]
                `shouldReturn` "matched hello\n"
        it "Can match empty list" $ do
            compileAndRun
                [r|
                    let main : IO = do
                        let list = []
                        when list of
                            [] -> println "empty"
                            (x:xs) -> println "not empty"
                            else -> println "other"
                        end
                    end
                |]
                `shouldReturn` "empty\n"
        it "Can match list with single element" $ do
            compileAndRun
                [r|
                    let main : IO = do
                        let list = [1]
                        when list of
                            [] -> println "empty"
                            [x] -> println "single"
                            (x:xs) -> println "cons"
                            else -> println "other"
                        end
                    end
                |]
                `shouldReturn` "single\n"
        it "Can match list with multiple elements" $ do
            compileAndRun
                [r|
                    let main : IO = do
                        let list = [1, 2, 3]
                        when list of
                            [] -> println "empty"
                            [x] -> println "single"
                            [x, y] -> println "two"
                            (x:y:rest) -> println "three or more"
                            else -> println "other"
                        end
                    end
                |]
                `shouldReturn` "three or more\n"
        it "Can match string as list of chars" $ do
            compileAndRun
                [r|
                    let main : IO = do
                        let str = "hello"
                        when str of
                            [] -> println "empty"
                            ['h', 'e', 'l', 'l', 'o'] -> println "matched hello"
                            (c:rest) -> println "starts with char"
                            else -> println "other"
                        end
                    end
                |]
                `shouldReturn` "matched hello\n"
        it "Can use when as function body" $ do
            compileAndRun
                [r|
                    let main : IO = when 5 of
                        1 -> println "one"
                        5 -> println "five"
                        else -> println "other"
                    end
                |]
                `shouldReturn` "five\n"
        it "Can use else clause" $ do
            compileAndRun
                [r|
                    let main : IO = do
                        let x = 99
                        when x of
                            1 -> println "one"
                            2 -> println "two"
                            else -> println "other"
                        end
                    end
                |]
                `shouldReturn` "other\n"
        it "Can handle nested when statements" $ do
            compileAndRun
                [r|
                    let main : IO = do
                        let x = 1
                        let y = 2
                        when x of
                            1 -> when y of
                                2 -> println "nested match"
                                else -> println "inner else"
                            end
                            else -> println "outer else"
                        end
                    end
                |]
                `shouldReturn` "nested match\n"
    describe "Variable shadowing" $ do
        it "Pattern matching variables shadow outer let bindings" $ do
            compileAndRun
                [r|
                    let main : IO = do
                        let x = 5
                        let list = [1, 2, 3]
                        when list of
                            (x:y:rest) -> do
                                println x
                                println y
                            end
                            else -> println "no match"
                        end
                    end
                |]
                `shouldReturn` "1\n2\n"
        it "Function parameters shadow outer variables" $ do
            compileAndRun
                [r|
                    let x = 10
                    let f (x: Int) : Int = x + 1
                    let main : IO = println (f 5)
                |]
                `shouldReturn` "6\n"
        it "Nested let bindings shadow outer variables" $ do
            compileAndRun
                [r|
                    let main : IO = do
                        let x = 1
                        let y = do
                            let x = 2
                            x
                        end
                        println x
                        println y
                    end
                |]
                `shouldReturn` "1\n2\n"
        it "Shadowing is properly scoped and doesn't leak" $ do
            compileAndRun
                [r|
                    let main : IO = do
                        let x = 1
                        when [10, 20] of
                            (x:y:rest) -> do
                                println x
                            end
                            else -> println "no match"
                        end
                        println x
                    end
                |]
                `shouldReturn` "10\n1\n"
        it "Multiple levels of shadowing in pattern matching" $ do
            compileAndRun
                [r|
                    let main : IO = do
                        let x = 100
                        let y = 200
                        when [1, 2, 3] of
                            (x:y:rest) -> do
                                println x
                                println y
                            end
                            else -> println "no match"
                        end
                        println x
                        println y
                    end
                |]
                `shouldReturn` "1\n2\n100\n200\n"
