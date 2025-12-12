module IntegrationSpec (spec) where

import BytecodeCompiler
    ( CompilerError (..)
    , buildModuleMap
    , compileProgram
    , initCompilerState
    , initCompilerStateWithModules
    , locateLabel
    , renderCompilerErrors
    )
import Control.Monad.State (evalStateT)
import Data.Functor
import Data.List (elemIndex, isInfixOf)
import Data.Map qualified as Map
import Data.Text qualified
import Data.Text qualified as T
import Data.Vector qualified as V
import ErrorRenderer (parseErrorBundleToSourceErrors, renderErrors)
import Optimizer
import Parser
    ( CompilerFlags (CompilerFlags, verboseMode)
    , parseProgram
    )
import Parser qualified
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (describe, it, pendingWith, shouldReturn, xit)
import Test.QuickCheck (Testable (property))
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

compileAndRun :: String -> IO String
compileAndRun prog = do
    let p = parseProgram (Data.Text.pack prog) Parser.initCompilerFlags
    case p of
        Left err -> error $ renderErrors (parseErrorBundleToSourceErrors err (Data.Text.pack prog)) prog
        Right program -> do
            let (Parser.Program exprs _) = program
            compilerOutput <- evalStateT (compileProgram program) (initCompilerState program)
            optimizedProgram <- case compilerOutput of
                Left bytecode -> pure (optimize bytecode)
                Right err -> error $ renderCompilerErrors err (Map.singleton "<input>" prog)
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
            vm <- runVMVM $ (initVM (V.fromList optimizedProgram)){pc = startPc, breakpoints = [], callStack = [StackFrame{returnAddress = 0, locals = []}, StackFrame{returnAddress = mainLabel, locals = []}], ioMode = VMBuffer, shouldExit = False}
            pure $ output $ ioBuffer vm

compileAndRunModules :: [(String, String)] -> IO String
compileAndRunModules modules = do
    withSystemTempDirectory "indigo-test" $ \tmpDir -> do
        filePaths <-
            mapM
                ( \(name, content) -> do
                    let fileName = name ++ ".in"
                    let filePath = tmpDir </> fileName
                    writeFile filePath content
                    return filePath
                )
                modules

        moduleMapResult <- buildModuleMap filePaths
        case moduleMapResult of
            Left err -> error $ "Failed to build module map: " ++ err
            Right moduleMap -> do
                let (mainName, mainContent) = head modules
                let mainFilePath = tmpDir </> mainName ++ ".in"
                let mainProg = parseProgram (T.pack mainContent) Parser.initCompilerFlags{Parser.needsMain = False}
                case mainProg of
                    Left err -> error $ renderErrors (parseErrorBundleToSourceErrors err (T.pack mainContent)) mainContent
                    Right program -> do
                        let (Parser.Program exprs _) = program
                        let state = initCompilerStateWithModules moduleMap program mainFilePath
                        compilerOutput <- evalStateT (compileProgram program) state
                        let fileContents = Map.fromList $ map (\(name, content) -> (tmpDir </> name ++ ".in", content)) modules
                        optimizedProgram <- case compilerOutput of
                            Left bytecode -> pure (optimize bytecode)
                            Right err -> error $ renderCompilerErrors err fileContents
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
                        vm <- runVMVM $ (initVM (V.fromList optimizedProgram)){pc = startPc, breakpoints = [], callStack = [StackFrame{returnAddress = 0, locals = []}, StackFrame{returnAddress = mainLabel, locals = []}], ioMode = VMBuffer, shouldExit = False}
                        pure $ output $ ioBuffer vm

spec = do
    describe "Hello World" $ do
        it "Should print Hello, world!" $ do
            compileAndRun "let main : IO<Unit> = println \"Hello, world!\"" `shouldReturn` "Hello, world!\n"
    -- describe "println" $ do
    -- it "Can print any string" $ do
    -- property $ \s -> compileAndRun ("let main : IO<Unit> = println " ++ show s) `shouldReturn` (s ++ "\n")
    describe "Operator precedence" $ do
        it "Has working precedence for multiplication" $ do
            compileAndRun "let main : IO<Unit> = println 1 + 2 * 3" `shouldReturn` "7\n"
        it "Has working precedence for brackets" $ do
            compileAndRun "let main : IO<Unit> = println (1 + 2) * 3" `shouldReturn` "9\n"
    describe "Pattern matching" $ do
        it "Can match Int" $ do
            compileAndRun
                [r|
                t :: Int -> Int
                t 1 = 2
                t 2 = 3
                t _ = 4
                let main : IO<Unit> = println (t 1)
            |]
                `shouldReturn` "2\n"
        it "Can match String" $ do
            compileAndRun
                [r|
                t :: String -> Int
                t "a" = 2
                t "b" = 3
                t _ = 4
                let main : IO<Unit> = println (t "a")
            |]
                `shouldReturn` "2\n"
        it "Can match struct" $
            do
                compileAndRun
                    [r|
                bind :: Any -> Any -> Any
                bind Some<Int>{value: x} f = f x
                bind None{} f = None{}
                bind _ = "Error: Invalid argument to bind."
                
                let main : IO<Unit> = do
                    println (bind Some<Int>{value: 5}, \x -> x + 1)
                    println (bind None{}, \x -> x + 1)
                end
            |]
                `shouldReturn` "6\nNone{__traits: [Monad,Optional]}\n"
        it "Handles multiple variables correctly" $ do
            compileAndRun
                [r|
                bla :: Int -> Int -> Int
                bla 1 x = 100 + x
                bla 2 x = 200 + x
                bla 3 x = 300 + x

                let main : IO<Unit> = do
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
                let main : IO<Unit> = println (t [1, 2, 3])
            |]
                `shouldReturn` "1\n"
        -- it "Can match last element of a list" $ do
        --     compileAndRun
        --         [r|
        --         t :: [Int] -> Int
        --         t [] = 0
        --         t [x] = x
        --         t (x:xs) = t xs
        --         let main : IO<Unit> = println (t [1, 2, 3])
        --     |]
        --         `shouldReturn` "3\n"
        describe "Lists" $ do
            it "Can return the first element of a list" $ do
                compileAndRun "t :: [Int] -> Int\nt (x:xs) = x\nlet main : IO<Unit> = println (t [1, 2, 3])" `shouldReturn` "1\n"
            it "Can return the first two" $ do
                compileAndRun "t :: [Int] -> Int\nt (x:y:xs) = x + y\nlet main : IO<Unit> = println (t [1, 2, 3])" `shouldReturn` "3\n"
            it "Can return the excess" $ do
                compileAndRun "t :: [Int] -> Int\nt (x:y:xs) = xs\nlet main : IO<Unit> = println (t [1, 2, 3])" `shouldReturn` "[3]\n"
            it "Can detect zero elements" $ do
                compileAndRun
                    [r|t :: [Int] -> Int
                                t [] = 0
                                t (x:xs) = x
                                let main : IO<Unit> = println (t [] as [Int])|]
                    `shouldReturn` "0\n"
        it "Can detect one element" $ do
            compileAndRun
                [r|t :: [Int] -> Int
                            t [] = 0
                            t (x:[]) = 1
                            t (x:xs) = x
                            let main : IO<Unit> = println (t [4])|]
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

                        let main : IO<Unit> = do
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

                        let main : IO<Unit> = do
                            println test Teacher{name: "Alice"}
                            println test Student{name: "Bob"}
                            println test Foo{name: "Charlie"}
                        end
                    |]
                    `shouldReturn` "Alice\nBob\nUnknown\n"
    describe "Prelude" $ do
        it "Can use map" $ do
            compileAndRun "let main : IO<Unit> = println (map (`+`1), [1, 2, 3])" `shouldReturn` "[2,3,4]\n"
        it "Can use sum on integers" $ do
            pendingWith "Needs further language support"
            compileAndRun "let main : IO<Unit> = println (sum [1, 2, 3])" `shouldReturn` "6\n"
        it "Can use sum on floats" $ do
            pendingWith "Needs further language support"
            compileAndRun "let main : IO<Unit> = println (sum [1.0, 2.0, 3.0])" `shouldReturn` "6.0\n"
        it "Can use foldl" $ do
            compileAndRun "let main : IO<Unit> = println (foldl (`+`), 0, [1, 2, 3])" `shouldReturn` "6\n"
    describe "Implicit casting" $ do
        it "Can cast from int to float" $ do
            compileAndRun [r|let main : IO<Unit> = println ^2 + 4.0|] `shouldReturn` "6.0\n"
            compileAndRun [r|let main : IO<Unit> = println 2.0 + ^4|] `shouldReturn` "6.0\n"
        it "Can cast from float to int" $ do
            compileAndRun [r|let main : IO<Unit> = println ^2.0 + 4|] `shouldReturn` "6\n"
            compileAndRun [r|let main : IO<Unit> = println 2 + ^4.0|] `shouldReturn` "6\n"
        it "Can cast from int to string" $ do
            compileAndRun [r|let main : IO<Unit> = println ^2 + "test"|] `shouldReturn` "2test\n"
        it "Can cast from string to int" $ do
            compileAndRun [r|let main : IO<Unit> = println ^"2" + 4|] `shouldReturn` "6\n"
    describe "Explicit casting" $ do
        it "Can cast from int to float" $ do
            compileAndRun [r|let main : IO<Unit> = println (2 as Float) + 4.0|] `shouldReturn` "6.0\n"
            compileAndRun [r|let main : IO<Unit> = println 2.0 + (4 as Float)|] `shouldReturn` "6.0\n"
        it "Can cast a function call" $ do
            compileAndRun
                [r|f :: Int -> Int
                            f x = x + 1
                            let main : IO<Unit> = println ((f 2) as Float) + 4.0|]
                `shouldReturn` "7.0\n"
    describe "Overloading" $ do
        xit "Can find function based on type" $ do
            compileAndRun
                [r|
                f :: Int -> Int
                f x = x + 1
                f :: String -> String
                f x = "'":x:"'"
                let main : IO<Unit> = do
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
                let main : IO<Unit> = do
                    println f [1, 2, 3]
                    println f ["test", "test2"]
                end|]
                `shouldReturn` "6\n[test,test2]\n"
    describe "Structs" $ do
        it "Can evaluate the point example" $ do
            compileAndRun
                [r|
                struct Point = (x: Int, y: Int)

                let main : IO<Unit> = do
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

                let main : IO<Unit> = do
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

                let main : IO<Unit> = do
                    let mauzi = Cat { name: "Mauzi" }
                    println (getName mauzi)
                end|]
                `shouldReturn` "Mauzi\n"
        xit "Can access fields via automatically generated functions" $ do
            compileAndRun
                [r|
                struct Dog = (name: String)
                struct Cat = (name: String)

                let main : IO<Unit> = do
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
                        makeNoise :: Self -> IO<Unit>
                    end

                    impl Animal for Dog = do
                        makeNoise self = println "Woof"
                    end

                    impl Animal for Cat = do
                        makeNoise self = println "Meow"
                    end

                    let main : IO<Unit> = do
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

                    let main : IO<Unit> = do
                      let bla = MMaybe { some: "hello", none: "" }
                      mbind bla, print
                    end
                    |]
                `shouldReturn` "hello"
        it "Trait with required properties - valid impl" $ do
            compileAndRun
                [r|
                    trait Printable = (name: String, age: Int) do
                        getName :: Self -> String
                    end

                    struct Person = (name: String, age: Int)

                    impl Printable for Person = do
                        getName self = self.name
                    end

                    let main : IO<Unit> = do
                        let p = Person { name: "Alice", age: 30 }
                        println (getName p)
                    end
                |]
                `shouldReturn` "Alice\n"
        it "Trait with required properties and methods - multiple methods" $ do
            compileAndRun
                [r|
                    trait Describable = (name: String, age: Int) do
                        describe :: Self -> String
                        isAdult :: Self -> Bool
                    end

                    struct Person = (name: String, age: Int)

                    impl Describable for Person = do
                        describe self = self.name ++ " is " ++ (self.age as String) ++ " years old"
                        isAdult self = self.age >= 18
                    end

                    let main : IO<Unit> = do
                        let p = Person { name: "Alice", age: 30 }
                        println (describe p)
                        println (isAdult p)
                    end
                |]
                `shouldReturn` "Alice is 30 years old\nTrue\n"
        it "Trait with required properties and methods - using 'is' clause" $ do
            compileAndRun
                [r|
                    trait Describable = (name: String, age: Int) do
                        describe :: Self -> String
                        isAdult :: Self -> Bool
                    end

                    struct Person = (name: String, age: Int) is Describable

                    impl Describable for Person = do
                        describe self = self.name ++ " is " ++ (self.age as String) ++ " years old"
                        isAdult self = self.age >= 18
                    end

                    let main : IO<Unit> = do
                        let p = Person { name: "Bob", age: 17 }
                        println (describe p)
                        println (isAdult p)
                    end
                |]
                `shouldReturn` "Bob is 17 years old\nFalse\n"
        it "Trait with required properties and methods - trait dispatch" $ do
            compileAndRun
                [r|
                    trait Describable = (name: String, age: Int) do
                        describe :: Self -> String
                    end

                    struct Person = (name: String, age: Int)
                    struct Employee = (name: String, age: Int, salary: Int)

                    impl Describable for Person = do
                        describe self = self.name ++ " is " ++ (self.age as String) ++ " years old"
                    end

                    impl Describable for Employee = do
                        describe self = self.name ++ " is " ++ (self.age as String) ++ " years old (Employee)"
                    end

                    let main : IO<Unit> = do
                        let p = Person { name: "Alice", age: 30 }
                        let e = Employee { name: "Bob", age: 25, salary: 50000 }
                        println (describe p)
                        println (describe e)
                    end
                |]
                `shouldReturn` "Alice is 30 years old\nBob is 25 years old (Employee)\n"
        it "Trait with required properties - missing property" $ do
            let prog =
                    [r|
                    trait Printable = (name: String, age: Int) do
                        getName :: Self -> String
                    end

                    struct Person = (name: String)

                    impl Printable for Person = do
                        getName self = self.name
                    end

                    let main : IO<Unit> = do
                        println "test"
                    end
                |]
            let p = parseProgram (Data.Text.pack prog) Parser.initCompilerFlags
            case p of
                Left _ -> return ()
                Right program -> do
                    result <- evalStateT (compileProgram program) (initCompilerState program)
                    case result of
                        Left _ -> error "Expected compilation error"
                        Right errs -> if any (\case CompilerError msg _ _ -> "Missing required property" `isInfixOf` msg; _ -> False) errs then return () else error $ "Wrong error: " ++ renderCompilerErrors errs (Map.singleton "<input>" prog)
        it "Trait with refinement - valid impl" $ do
            compileAndRun
                [r|
                    trait PositiveNumber satisfies (x > 0) = do
                        getValue :: Self -> Int
                    end

                    struct PosInt = (x: Int)

                    impl PositiveNumber for PosInt = do
                        getValue self = self.x
                    end

                    let main : IO<Unit> = do
                        let p = PosInt { x: 5 }
                        println ((getValue p) as String)
                    end
                |]
                `shouldReturn` "5\n"
        it "Trait with refinement - failing impl" $ do
            let prog =
                    [r|
                    trait PositiveNumber satisfies (x > 0) = do
                        getValue :: Self -> Int
                    end

                    struct Int = (x: Int)

                    impl PositiveNumber for Int = do
                        getValue self = self.x
                    end

                    let main : IO<Unit> = do
                        let i = Int { x: 0 }
                        println ((getValue i) as String)
                    end
                |]
            let p = parseProgram (Data.Text.pack prog) Parser.initCompilerFlags
            case p of
                Left _ -> return ()
                Right program -> do
                    result <- evalStateT (compileProgram program) (initCompilerState program)
                    case result of
                        Left _ -> error "Expected compilation error"
                        Right errs -> if any (\case CompilerError msg _ _ -> "Trait 'PositiveNumber' refinement failed" `isInfixOf` msg; _ -> False) errs then return () else error $ "Wrong error: " ++ renderCompilerErrors errs (Map.singleton "<input>" prog)
        it "Trait with both required properties and refinement" $ do
            compileAndRun
                [r|
                    trait ValidPerson = (name: String, age: Int) satisfies (age > 0) do
                        getName :: Self -> String
                    end

                    struct Person = (name: String, age: Int) satisfies (age > 0)

                    impl ValidPerson for Person = do
                        getName self = self.name
                    end

                    let main : IO<Unit> = do
                        let p = Person { name: "Bob", age: 25 }
                        println (getName p)
                    end
                |]
                `shouldReturn` "Bob\n"
        it "Struct with 'is' clause - trait with required properties (valid)" $ do
            compileAndRun
                [r|
                    trait Printable = (name: String, age: Int) do
                        getName :: Self -> String
                    end

                    struct Person = (name: String, age: Int) is Printable

                    impl Printable for Person = do
                        getName self = self.name
                    end

                    let main : IO<Unit> = do
                        let p = Person { name: "Alice", age: 30 }
                        println (getName p)
                    end
                |]
                `shouldReturn` "Alice\n"
        it "Struct with 'is' clause - trait with required properties (missing property)" $ do
            let prog =
                    [r|
                    trait Printable = (name: String, age: Int) do
                        getName :: Self -> String
                    end

                    struct Person = (name: String) is Printable

                    impl Printable for Person = do
                        getName self = self.name
                    end

                    let main : IO<Unit> = do
                        println "test"
                    end
                |]
            let p = parseProgram (Data.Text.pack prog) Parser.initCompilerFlags
            case p of
                Left _ -> return ()
                Right program -> do
                    result <- evalStateT (compileProgram program) (initCompilerState program)
                    case result of
                        Left _ -> error "Expected compilation error"
                        Right errs -> if any (\case CompilerError msg _ _ -> "Missing required property" `isInfixOf` msg; _ -> False) errs then return () else error $ "Wrong error: " ++ renderCompilerErrors errs (Map.singleton "<input>" prog)
        it "Struct with 'is' clause - trait with required properties (type mismatch)" $ do
            let prog =
                    [r|
                    trait Printable = (name: String, age: Int) do
                        getName :: Self -> String
                    end

                    struct Person = (name: String, age: String) is Printable

                    impl Printable for Person = do
                        getName self = self.name
                    end

                    let main : IO<Unit> = do
                        println "test"
                    end
                |]
            let p = parseProgram (Data.Text.pack prog) Parser.initCompilerFlags
            case p of
                Left _ -> return ()
                Right program -> do
                    result <- evalStateT (compileProgram program) (initCompilerState program)
                    case result of
                        Left _ -> error "Expected compilation error"
                        Right errs -> if any (\case CompilerError msg _ _ -> "type mismatch" `isInfixOf` msg && "Required property" `isInfixOf` msg; _ -> False) errs then return () else error $ "Wrong error: " ++ renderCompilerErrors errs (Map.singleton "<input>" prog)
        it "Struct with 'is' clause - trait with refinement (valid)" $ do
            compileAndRun
                [r|
                    trait PositiveNumber satisfies (x > 0) = do
                        getValue :: Self -> Int
                    end

                    struct PosInt = (x: Int) is PositiveNumber

                    impl PositiveNumber for PosInt = do
                        getValue self = self.x
                    end

                    let main : IO<Unit> = do
                        let p = PosInt { x: 5 }
                        println ((getValue p) as String)
                    end
                |]
                `shouldReturn` "5\n"
        it "Struct with 'is' clause - trait with refinement (failing)" $ do
            let prog =
                    [r|
                    trait PositiveNumber satisfies (x > 0) = do
                        getValue :: Self -> Int
                    end

                    struct Int = (x: Int) is PositiveNumber

                    impl PositiveNumber for Int = do
                        getValue self = self.x
                    end

                    let main : IO<Unit> = do
                        let i = Int { x: 0 }
                        println ((getValue i) as String)
                    end
                |]
            let p = parseProgram (Data.Text.pack prog) Parser.initCompilerFlags
            case p of
                Left _ -> return ()
                Right program -> do
                    result <- evalStateT (compileProgram program) (initCompilerState program)
                    case result of
                        Left _ -> error "Expected compilation error"
                        Right errs -> if any (\case CompilerError msg _ _ -> "Trait 'PositiveNumber' refinement failed" `isInfixOf` msg; _ -> False) errs then return () else error $ "Wrong error: " ++ renderCompilerErrors errs (Map.singleton "<input>" prog)
        it "Struct with 'is' clause - trait with both required properties and refinement (valid)" $ do
            compileAndRun
                [r|
                    trait ValidPerson = (name: String, age: Int) satisfies (age > 0) do
                        getName :: Self -> String
                    end

                    struct Person = (name: String, age: Int) satisfies (age > 0) is ValidPerson

                    impl ValidPerson for Person = do
                        getName self = self.name
                    end

                    let main : IO<Unit> = do
                        let p = Person { name: "Bob", age: 25 }
                        println (getName p)
                    end
                |]
                `shouldReturn` "Bob\n"
        it "Static dispatch" $ do
            compileAndRun
                [r|
                    printNumbersAndSum :: IO<Unit>
                    printNumbersAndSum = println "x"

                    main :: IO<Unit>
                    main = do
                        printNumbersAndSum
                    end
                |]
                `shouldReturn` "x\n"
        it "Can use parametric trait with impl" $ do
            compileAndRun
                [r|
                    trait Monad<T> = do
                        bind :: Self -> (Any -> Self) -> Self
                        return :: Any -> Self
                    end

                    struct Optional = (value: Any)

                    impl Monad<Optional> for Optional = do
                        bind Optional{value: x} f = f x
                        bind None{} f = None{}
                        return x = Optional{value: x}
                    end

                    let main : IO<Unit> = do
                        let opt = Optional{value: 42}
                        println opt.value
                    end
                |]
                `shouldReturn` "42\n"
        it "Can use parametric trait with multiple type parameters" $ do
            compileAndRun
                [r|
                    trait Pair<T, U> = do
                        first :: Self -> T
                        second :: Self -> U
                    end

                    struct IntStringPair = (first: Int, second: String)

                    impl Pair<Int, String> for IntStringPair = do
                        first self = self.first
                        second self = self.second
                    end

                    let main : IO<Unit> = do
                        let p = IntStringPair{first: 42, second: "hello"}
                        println p.first
                        println p.second
                    end
                |]
                `shouldReturn` "42\nhello\n"
        it "Can use generic IO with Int" $ do
            compileAndRun
                [r|
                    let main : IO<Unit> = do
                        let io = IO<Int>{inner: 42}
                        println io.inner
                    end
                |]
                `shouldReturn` "42\n"
        it "Can use generic IO with String" $ do
            compileAndRun
                [r|
                    let main : IO<Unit> = do
                        let io = IO<String>{inner: "hello"}
                        println io.inner
                    end
                |]
                `shouldReturn` "hello\n"
        it "Can use generic Monad with IO<Int>" $ do
            compileAndRun
                [r|
                    let main : IO<Unit> = do
                        let io1 = IO<Int>{inner: 5}
                        let io2 = bind io1, \x -> IO<Int>{inner: x + 1}
                        println io2.inner
                    end
                |]
                `shouldReturn` "6\n"
        it "Can use generic Monad return with IO" $ do
            compileAndRun
                [r|
                    let main : IO<Unit> = do
                        let io : IO<Int> = return 42
                        println io.inner
                    end
                |]
                `shouldReturn` "42\n"
        it "Can use generic Monad return with Some" $ do
            compileAndRun
                [r|
                    let main : IO<Unit> = do
                        let some : Some<Int> = return 42
                        println some.value
                    end
                |]
                `shouldReturn` "42\n"
        it "Can chain generic Monad bind with IO" $ do
            compileAndRun
                [r|
                    let main : IO<Unit> = do
                        let io1 : IO<Int> = return 1
                        let io2 : IO<Int> = bind io1, \x -> return (x + 1)
                        let io3 : IO<Int> = bind io2, \x -> return (x * 2)
                        println io3.inner
                    end
                |]
                `shouldReturn` "4\n"
    describe "Lambdas" $ do
        it "Can use a lambda in the map function" $ do
            compileAndRun
                [r|
                    let main : IO<Unit> = do
                        println map (\x -> x + 1), [1, 2, 3]
                    end
                |]
                `shouldReturn` "[2,3,4]\n"
        it "Can use a lambda calling a nested function" $ do
            compileAndRun
                [r|
                    let main : IO<Unit> = do
                        let square (x: Int) : Int = x * x
                        let toThird (x: Int) : Int = x * square x
                        println map (\x -> toThird x as Int), [1, 2, 3]
                    end
                |]
                `shouldReturn` "[1,8,27]\n"
        it "Can use strict values in lambda" $ do
            compileAndRun
                [r|
                    let main : IO<Unit> = do
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

                let main : IO<Unit> = do
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

                let main : IO<Unit> = do
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

                let main : IO<Unit> = do
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

                let main : IO<Unit> = println (compose increase, increase) 2
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

                let main : IO<Unit> = println increaseByTwo 2
                |]
                `shouldReturn` "4\n"
    describe "No main" $ do
        it "Hello World" $ do
            compileAndRun
                [r|
                    let main : IO<Unit> = println "Hello, World!"
                |]
                `shouldReturn` "Hello, World!\n"
        it "Functional" $ do
            compileAndRun
                [r|
                    let main : IO<Unit> = print (map (`*`3), [2, 4, 6, 8])
                |]
                `shouldReturn` "[6,12,18,24]"
        it "Structured" $ do
            compileAndRun
                [r|
                    struct Person = (name: String, age: Int)
                    let peter = Person { name: "Peter", age: 24 }
                    let main : IO<Unit> = println peter.name ++ " is " ++ (peter.age) as String ++ " years old"
                |]
                `shouldReturn` "Peter is 24 years old\n"
        it "Pattern matching" $ do
            compileAndRun
                [r|
                    let head ([]: [Any]) = 0
                    let head ((x:xs): [Any]) = x
                    let main : IO<Unit> = print head [1,2,3]
                |]
                `shouldReturn` "1"
        it "\"99\" bottles of beer" $ do
            compileAndRun
                [r|
                    let bottles (i: Int) : IO<Unit> = do
                        if i > 0 then do
                            println i as String ++ " bottles of beer on the wall, " ++ i as String ++ " bottles of beer."
                            println "Take one down and pass it around, " ++ ((i) - 1) as String ++ " bottles of beer on the wall.\n"
                            bottles (i)-1
                        else do
                            println "No more bottles of beer on the wall, no more bottles of beer."
                            println "Go to the store and buy some more, 99 bottles of beer on the wall."
                        end
                    end

                    let main : IO<Unit> = bottles 3
                |]
                `shouldReturn` "3 bottles of beer on the wall, 3 bottles of beer.\nTake one down and pass it around, 2 bottles of beer on the wall.\n\n2 bottles of beer on the wall, 2 bottles of beer.\nTake one down and pass it around, 1 bottles of beer on the wall.\n\n1 bottles of beer on the wall, 1 bottles of beer.\nTake one down and pass it around, 0 bottles of beer on the wall.\n\nNo more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall.\n"
    describe "when statements" $ do
        it "Can match simple integer values" $ do
            compileAndRun
                [r|
                    let main : IO<Unit> = do
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
                    let main : IO<Unit> = do
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
                    let main : IO<Unit> = do
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
                    let main : IO<Unit> = do
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
                    let main : IO<Unit> = do
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
                    let main : IO<Unit> = do
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
                    let main : IO<Unit> = do
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
                    let main : IO<Unit> = when 5 of
                        1 -> println "one"
                        5 -> println "five"
                        else -> println "other"
                    end
                |]
                `shouldReturn` "five\n"
        it "Can use else clause" $ do
            compileAndRun
                [r|
                    let main : IO<Unit> = do
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
                    let main : IO<Unit> = do
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
                    let main : IO<Unit> = do
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
                    let main : IO<Unit> = println (f 5)
                |]
                `shouldReturn` "6\n"
        it "Nested let bindings shadow outer variables" $ do
            compileAndRun
                [r|
                    let main : IO<Unit> = do
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
                    let main : IO<Unit> = do
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
                    let main : IO<Unit> = do
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
    describe "Tuples" $ do
        it "Should create and access tuple elements" $ do
            compileAndRun
                [r|
                let main : IO<Unit> = do
                    let t = (1, "hello", True)
                    println t.0
                    println t.1
                    println t.2
                end
            |]
                `shouldReturn` "1\nhello\nTrue\n"
        it "Should work with tuple type annotations" $ do
            compileAndRun
                [r|
                let main : IO<Unit> = do
                    let t: (Int, String) = (42, "test")
                    println t.0
                    println t.1
                end
            |]
                `shouldReturn` "42\ntest\n"
        it "Should work with nested tuples" $ do
            compileAndRun
                [r|
                let main : IO<Unit> = do
                    let t = ((1, 2), (3, 4))
                    let first = t.0
                    let second = t.1
                    println first.0
                    println first.1
                    println second.0
                    println second.1
                end
            |]
                `shouldReturn` "1\n2\n3\n4\n"
        it "Should work with tuples in function parameters" $ do
            compileAndRun
                [r|
                f :: (Int, String) -> Int
                f t = t.0
                let main : IO<Unit> = println (f (42, "test"))
            |]
                `shouldReturn` "42\n"
        it "Should work with tuples as function return types" $ do
            compileAndRun
                [r|
                f :: Int -> (Int, Int)
                f x = (x, x * 2)
                let main : IO<Unit> = println (f 5).0
            |]
                `shouldReturn` "5\n"
        it "Should work with tuple comparison" $ do
            compileAndRun
                [r|
                let main : IO<Unit> = println ((1, 2) == (1, 2))
            |]
                `shouldReturn` "True\n"
        it "Should work with tuples in lists" $ do
            compileAndRun
                [r|
                let main : IO<Unit> = do
                    let list = [(1, 2), (3, 4)]
                    let first = list.0
                    let second = list.1
                    println first.0
                    println second.1
                end
            |]
                `shouldReturn` "1\n4\n"
        it "Should work with tuple pattern matching in function definitions" $ do
            compileAndRun
                [r|
                f :: (Int, String) -> Int
                f (x, y) = x
                let main : IO<Unit> = println (f (42, "test"))
            |]
                `shouldReturn` "42\n"
        it "Should work with tuple pattern matching extracting both values" $ do
            compileAndRun
                [r|
                f :: (Int, String) -> String
                f (x, y) = y
                let main : IO<Unit> = println (f (42, "hello"))
            |]
                `shouldReturn` "hello\n"
        it "Should work with nested tuple pattern matching" $ do
            compileAndRun
                [r|
                f :: ((Int, Int), (Int, Int)) -> Int
                f ((a, b), (c, d)) = a + b + c + d
                let main : IO<Unit> = println (f ((1, 2), (3, 4)))
            |]
                `shouldReturn` "10\n"
        it "Should work with tuple pattern matching in when statements" $ do
            compileAndRun
                [r|
                let main : IO<Unit> = do
                    let t = (1, "hello")
                    when t of
                        (1, "hello") -> println "matched (1, hello)"
                        (2, "world") -> println "matched (2, world)"
                        (x, y) -> println "matched other"
                    end
                end
            |]
                `shouldReturn` "matched (1, hello)\n"
        it "Should work with tuple pattern matching with variables in when" $ do
            compileAndRun
                [r|
                let main : IO<Unit> = do
                    let t = (42, "test")
                    when t of
                        (x, y) -> println x
                    end
                end
            |]
                `shouldReturn` "42\n"
        it "Should work with tuple pattern matching with mixed literals and variables" $ do
            compileAndRun
                [r|
                f :: (Int, String) -> String
                f (1, y) = y
                f (2, y) = "two: " ++ y
                f (x, y) = "other"
                let main : IO<Unit> = do
                    println (f (1, "one"))
                    println (f (2, "two"))
                    println (f (3, "three"))
                end
            |]
                `shouldReturn` "one\ntwo: two\nother\n"
        it "Should handle tuple pattern matching where first pattern fails but later pattern matches" $ do
            compileAndRun
                [r|
                let main : IO<Unit> = do
                    let t = (12, "hello")
                    when t of
                        (1, "hello") -> println "matched (1, hello)"
                        (2, "world") -> println "matched (2, world)"
                        (x, y) -> do
                            println "matched other: "
                            println x
                            println y
                        end
                    end
                end
            |]
                `shouldReturn` "matched other: \n12\nhello\n"
        it "Should handle tuple pattern matching with multiple failing patterns before a match" $ do
            compileAndRun
                [r|
                let main : IO<Unit> = do
                    let t = (99, "test")
                    when t of
                        (1, "a") -> println "one"
                        (2, "b") -> println "two"
                        (3, "c") -> println "three"
                        (99, "test") -> println "matched 99"
                        (x, y) -> println "other"
                    end
                end
            |]
                `shouldReturn` "matched 99\n"
        it "Should handle nested tuple pattern matching" $ do
            compileAndRun
                [r|
                let main : IO<Unit> = do
                    let t = ((1, 2), (3, 4))
                    when t of
                        ((1, 2), (3, 4)) -> println "matched nested"
                        ((1, 2), (x, y)) -> println "matched partial"
                        (x, y) -> println "matched outer"
                    end
                end
            |]
                `shouldReturn` "matched nested\n"
        it "Should handle tuple pattern matching with mixed literal/variable patterns that fail early" $ do
            compileAndRun
                [r|
                let main : IO<Unit> = do
                    let t = (5, "five")
                    when t of
                        (1, x) -> println "one"
                        (2, x) -> println "two"
                        (5, "five") -> println "matched five"
                        (x, y) -> println "other"
                    end
                end
            |]
                `shouldReturn` "matched five\n"
    describe "Imports" $ do
        it "Should import and use qualified functions with single argument" $ do
            compileAndRunModules
                [
                    ( "Module1"
                    , [r|
module Module1

import qualified Module2

let main : IO<Unit> = do
    let result = Module2.square 5
    println result
end
|]
                    )
                ,
                    ( "Module2"
                    , [r|
module Module2

let square (x: Int) : Int = x * x
|]
                    )
                ]
                `shouldReturn` "25\n"
        it "Should import multiple functions from a module" $ do
            compileAndRunModules
                [
                    ( "Module1"
                    , [r|
module Module1

import qualified Module2

let main : IO<Unit> = do
    let squared = Module2.square 4
    let doubled = Module2.double 5
    println squared
    println doubled
end
|]
                    )
                ,
                    ( "Module2"
                    , [r|
module Module2

let square (x: Int) : Int = x * x
let double (x: Int) : Int = x * 2
|]
                    )
                ]
                `shouldReturn` "16\n10\n"
        it "Should import and use string functions" $ do
            compileAndRunModules
                [
                    ( "Main"
                    , [r|
module Main

import qualified Utils

let main : IO<Unit> = do
    let greeting = Utils.greet "Alice"
    println greeting
end
|]
                    )
                ,
                    ( "Utils"
                    , [r|
module Utils

let greet (name: String) : String = "Hello, " ++ name ++ "!"
|]
                    )
                ]
                `shouldReturn` "Hello, Alice!\n"
        it "Should handle module without explicit declaration" $ do
            compileAndRunModules
                [
                    ( "Main"
                    , [r|
import qualified Math

let main : IO<Unit> = println Math.square 5
|]
                    )
                ,
                    ( "Math"
                    , [r|
let square (x: Int) : Int = x * x
|]
                    )
                ]
                `shouldReturn` "25\n"
        it "Should import module with basic function" $ do
            compileAndRunModules
                [
                    ( "Main"
                    , [r|
module Main

import qualified Math

let main : IO<Unit> = do
    println (Math.double 21)
end
|]
                    )
                ,
                    ( "Math"
                    , [r|
module Math

let double (x: Int) : Int = x * 2
|]
                    )
                ]
                `shouldReturn` "42\n"
        it "Should import module and use prelude functions in imported module" $ do
            compileAndRunModules
                [
                    ( "Main"
                    , [r|
module Main

import qualified Lib

let main : IO<Unit> = do
    println (Lib.sumList [1, 2, 3])
end
|]
                    )
                ,
                    ( "Lib"
                    , [r|
module Lib

let sumList (xs: [Int]) : Int = foldl (`+`), 0, xs
|]
                    )
                ]
                `shouldReturn` "6\n"
        it "Should import module with helper function calling other module functions" $ do
            compileAndRunModules
                [
                    ( "Main"
                    , [r|
module Main

import qualified Math

let main : IO<Unit> = do
    println (Math.square 5)
    println (Math.cube 3)
end
|]
                    )
                ,
                    ( "Math"
                    , [r|
module Math

let square (x: Int) : Int = x * x
let cube (x: Int) : Int = x * (square x)
|]
                    )
                ]
                `shouldReturn` "25\n27\n"
        it "Should correctly not mangle prelude function calls in imported code" $ do
            compileAndRunModules
                [
                    ( "Main"
                    , [r|
module Main

import qualified Utils

let main : IO<Unit> = do
    println (Utils.formatList [1, 2, 3])
end
|]
                    )
                ,
                    ( "Utils"
                    , [r|
module Utils

let formatList (xs: [Int]) : String = do
    let doubled = map (\x -> x * 2), xs
    let total = foldl (`+`), 0, doubled
    (total as String) ++ " is the total"
end
|]
                    )
                ]
                `shouldReturn` "12 is the total\n"
