module BytecodeCompilerSpec (spec) where

import AST qualified
import BytecodeCompiler
import Control.Exception (SomeException)
import Control.Monad (join)
import Control.Monad.State (evalStateT, liftIO)
import Data.Functor ((<&>))
import Data.List (isInfixOf)
import Data.Map qualified as Map
import Data.Text qualified
import Debug.Trace
import ErrorRenderer (parseErrorBundleToSourceErrors, renderErrors)
import Foreign
import GHC.IO (unsafePerformIO)
import Parser qualified
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Text.RawString.QQ (r)
import Util
import VM (Action (..), Data (..), Instruction (..))

instance Arbitrary WordPtr where
    arbitrary = fromIntegral <$> (arbitrary :: Gen Word64)
    shrink x = [x]

instance Arbitrary Parser.Type where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary VM.Data where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary VM.Action where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary VM.Instruction where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary BytecodeCompiler.Function where
    arbitrary = genericArbitrary
    shrink = genericShrink

compile :: String -> IO [VM.Instruction]
compile prog = do
    let p = Parser.parseProgram (Data.Text.pack prog) Parser.initCompilerFlags
    case p of
        Left err -> error $ renderErrors (parseErrorBundleToSourceErrors err (Data.Text.pack prog)) prog
        Right program ->
            evalStateT
                ( compileProgramBare program >>= \case
                    Right err -> do
                        liftIO $ compileFail "<input>" err (Map.singleton "<input>" prog)
                        error ""
                    Left p -> return p
                )
                (initCompilerState program)
                    { funcDecs = [AST.FuncDec "+" [AST.StructT "Int" [], AST.StructT "Int" [], AST.StructT "Int" []] [] AST.anyPosition]
                    }

compileWithErrors :: String -> IO (Either [BytecodeCompiler.CompilerError] [VM.Instruction])
compileWithErrors prog = do
    let p = Parser.parseProgram (Data.Text.pack prog) Parser.initCompilerFlags
    case p of
        Left err -> error $ renderErrors (parseErrorBundleToSourceErrors err (Data.Text.pack prog)) prog
        Right program -> do
            result <-
                evalStateT
                    (compileProgram program)
                    (initCompilerState program)
                        { funcDecs = [AST.FuncDec "+" [AST.StructT "Int" [], AST.StructT "Int" [], AST.StructT "Int" []] [] AST.anyPosition]
                        }
            return $ case result of
                Left instructions -> Right instructions
                Right errors -> Left errors

spec :: Spec
spec = do
    describe "Addition" $ do
        it "Should compile 2+4" $
            compile [r|2+4|]
                `shouldReturn` [Label "main", StoreSideStack, Push 2, LStore "__op_a_0", Push 4, LStore "__op_b_0", LLoad "__op_a_0", LLoad "__op_b_0", Call "+", ClearSideStack, Push $ DInt 0, Exit]
        it "Should work properly with function calls" $
            compile
                [r|
                f :: Int -> Int
                f x = x + 1
                let main : IO<Unit> = unsafePrint (f 2) + 4|]
                `shouldReturn` [Label "f#0", StoreSideStack, LStore "x", LLoad "x", LStore "__op_a_1", Push 1, LStore "__op_b_1", LLoad "__op_a_1", LLoad "__op_b_1", Call "+", ClearSideStack, Ret, Label "main", StoreSideStack, Push 2, Call "f#0", LStore "__op_a_2", Push 4, LStore "__op_b_2", LLoad "__op_a_2", LLoad "__op_b_2", Call "+", Builtin Print, ClearSideStack, Push $ DInt 0, Exit]
    describe "Hello World" $
        it "Should print Hello, world!" $
            compile [r|let main : IO<Unit> = unsafePrint "Hello, world!"|]
                `shouldReturn` [Label "main", StoreSideStack, Push $ DString "Hello, world!", Builtin Print, ClearSideStack, Push $ DInt 0, Exit]
    -- describe "Implicit casting" $ do
    --     it "Should cast from int to float" $ do
    --         compile [r|let main : IO<Unit> = unsafePrint ^2 + 4.0|]
    --             `shouldReturn` [Label "main", StoreSideStack, Meta "flex", Push 2, LStore "__op_a_0", Push 4.0, LStore "__op_b_0", LLoad "__op_a_0", LLoad "__op_b_0", Cast, Push 4.0, Call "+", Builtin Print, ClearSideStack, Push $ DInt 0, Exit]
    --         compile [r|let main : IO<Unit> = unsafePrint 2.0 + ^4|]
    --             `shouldReturn` [Label "main", StoreSideStack, Push 2.0, LStore "__op_a_0", Meta "flex", Push 4, LStore "__op_b_0", LLoad "__op_a_0", LLoad "__op_b_0", Swp, Cast, Push 2.0, Call "+", Builtin Print, ClearSideStack, Push $ DInt 0, Exit]
    describe "Explicit casting" $ do
        it "Can cast from int to float" $
            compile [r|2 as Float|]
                `shouldReturn` [Label "main", StoreSideStack, Push 2, Push 0.0, Cast, ClearSideStack, Push $ DInt 0, Exit]
        it "Casts are compatible with binary operations" $
            compile [r|(2 as Float) + 4.0|]
                `shouldReturn` [Label "main", StoreSideStack, Push 2, Push 0.0, Cast, LStore "__op_a_0", Push 4.0, LStore "__op_b_0", LLoad "__op_a_0", LLoad "__op_b_0", Call "+", ClearSideStack, Push $ DInt 0, Exit]
    describe "Value structs" $ do
        it "Can cast to value struct without refinement" $ do
            result <-
                compileWithErrors
                    [r|
                value struct PositiveInt = (num: Int)
                let main : IO<Unit> = unsafePrint (42 as PositiveInt)
            |]
            case result of
                Right _ -> return ()
                Left errors -> expectationFailure $ "Should compile successfully, but got errors: " ++ show errors
        it "Can cast to value struct with refinement (passing)" $ do
            result <-
                compileWithErrors
                    [r|
                value struct EvenNumber = (num: Int) satisfies ((num % 2) == 0)
                let main : IO<Unit> = unsafePrint (12 as EvenNumber)
            |]
            case result of
                Right _ -> return ()
                Left errors -> expectationFailure $ "Should compile successfully, but got errors: " ++ show errors
        it "Fails to cast to value struct with refinement (failing)" $ do
            result <-
                compileWithErrors
                    [r|
                value struct EvenNumber = (num: Int) satisfies ((num % 2) == 0)
                let main : IO<Unit> = unsafePrint (5 as EvenNumber)
            |]
            case result of
                Left errors -> not (null errors) `shouldBe` True
                Right _ -> expectationFailure "Should fail with refinement error"
        it "Fails to cast incompatible type to value struct" $ do
            result <-
                compileWithErrors
                    [r|
                value struct PositiveInt = (num: Int)
                let main : IO<Unit> = unsafePrint ("hello" as PositiveInt)
            |]
            case result of
                Left errors -> not (null errors) `shouldBe` True
                Right _ -> expectationFailure "Should fail with type mismatch error"
    describe "Struct field type checking" $ do
        it "Fails when field type doesn't match" $ do
            result <-
                compileWithErrors
                    [r|
                struct Person = (name: String, age: Int)
                let main : IO<Unit> = unsafePrint (Person{name: "Alice", age: "30"})
            |]
            case result of
                Left errors -> length errors `shouldBe` 1
                Right _ -> expectationFailure "Should fail with type mismatch error"
        it "Succeeds when field types match" $ do
            result <-
                compileWithErrors
                    [r|
                struct Person = (name: String, age: Int)
                let main : IO<Unit> = unsafePrint (Person{name: "Alice", age: 30})
            |]
            case result of
                Right _ -> return ()
                Left errors -> expectationFailure $ "Should compile successfully, but got errors: " ++ show errors
    xdescribe "typesMatch" $ do
        it "Should be true for exact matches" $
            property $
                \t f ->
                    typesMatch f{types = [t]} [t]
                        `shouldBe` True
        it "Should be true for exact matches with multiple types" $
            property $
                \t1 t2 f ->
                    typesMatch f{types = [t1, t2]} [t1, t2]
                        `shouldBe` True
        it "Should be false for exact matches with multiple types in different order" $
            property $
                \t1 t2 f ->
                    Parser.Any
                        `notElem` [t1, t2]
                        && t1
                            /= t2
                        ==> typesMatch f{types = [t1, t2]} [t2, t1]
                            `shouldBe` False
        it "Should be true for Any" $
            property $
                \t f ->
                    typesMatch f{types = [Parser.Any]} [t]
                        `shouldBe` True
        it "Should be true for partial matches" $
            property $
                \t1 t2 f ->
                    typesMatch f{types = [t1, t2]} [t1]
                        `shouldBe` True
        it "Should be false for more arguments than types" $
            property $
                \t1 t2 f ->
                    typesMatch f{types = [t1]} [t1, t2]
                        `shouldBe` False
    describe "Refinement types" $ do
        it "Should compile successfully when refinement passes" $ do
            result <-
                compileWithErrors
                    [r|
                struct Age = (value: Int) satisfies (value >= 0)
                let main : IO<Unit> = do
                    let a = Age{value: 5}
                end
                |]
            case result of
                Left errors ->
                    let errorMessages = map BytecodeCompiler.errorMessage errors
                     in if any ("Refinement failed" `isInfixOf`) errorMessages
                            then expectationFailure $ "Expected successful compilation, got refinement error: " ++ show errors
                            else expectationFailure $ "Expected successful compilation, got errors: " ++ show errors
                Right _ -> return ()
        it "Should produce error when refinement fails" $ do
            result <-
                compileWithErrors
                    [r|
                struct Age = (value: Int) satisfies (value >= 0)
                let main : IO<Unit> = do
                    let a = Age{value: 0 - 1}
                end
                |]
            case result of
                Left errors -> do
                    let errorMessages = map BytecodeCompiler.errorMessage errors
                    any ("Refinement failed" `isInfixOf`) errorMessages
                        `shouldBe` True
                Right _ -> expectationFailure "Expected compilation error for failed refinement"
        it "Should compile normally when struct has no refinement" $ do
            result <-
                compileWithErrors
                    [r|
                struct Person = (name: String)
                let main : IO<Unit> = do
                    let p = Person{name: "Alice"}
                end
                |]
            case result of
                Left errors -> expectationFailure $ "Expected successful compilation, got errors: " ++ show errors
                Right _ -> return ()
        it "Should compile refinements with equality" $ do
            result <-
                compileWithErrors
                    [r|
                struct Person = (name: String) satisfies (name == "Alice")
                let main : IO<Unit> = do
                    let p = Person{name: "Alice"}
                end
                |]
            case result of
                Left errors -> expectationFailure $ "Expected successful compilation, got errors: " ++ show errors
                Right _ -> return ()
        it "Should compile successfully when refinement passes with local bindings" $ do
            result <-
                compileWithErrors
                    [r|
                struct Age = (value: Int) satisfies (value >= 0)
                let main : IO<Unit> = do
                    let ageValue = 25
                    let a = Age{value: ageValue}
                end
                |]
            case result of
                Left errors ->
                    let errorMessages = map BytecodeCompiler.errorMessage errors
                     in if any ("Refinement failed" `isInfixOf`) errorMessages
                            then expectationFailure $ "Expected successful compilation, got refinement error: " ++ show errors
                            else expectationFailure $ "Expected successful compilation, got errors: " ++ show errors
                Right _ -> return ()
        it "Should produce error when refinement fails with local bindings" $ do
            result <-
                compileWithErrors
                    [r|
                struct Age = (value: Int) satisfies (value >= 0)
                let main : IO<Unit> = do
                    let ageValue = 0 - 5
                    let a = Age{value: ageValue}
                end
                |]
            case result of
                Left errors -> do
                    let errorMessages = map BytecodeCompiler.errorMessage errors
                    any ("Refinement failed" `isInfixOf`) errorMessages
                        `shouldBe` True
                Right _ -> expectationFailure "Expected compilation error for failed refinement with local bindings"
        it "Should compile successfully with multiple local bindings in refinement" $ do
            result <-
                compileWithErrors
                    [r|
                let notJohn (x: String): Boolean = x != "John"
                struct Teacher = (name: String, age: Int) satisfies ((notJohn name) && age > 18)
                let main : IO<Unit> = do
                    let hisAge = 19
                    let otherName = "Johnny"
                    let t = Teacher { name: otherName, age: hisAge }
                end
                |]
            case result of
                Left errors ->
                    let errorMessages = map BytecodeCompiler.errorMessage errors
                     in if any ("Refinement failed" `isInfixOf`) errorMessages
                            then expectationFailure $ "Expected successful compilation, got refinement error: " ++ show errors
                            else expectationFailure $ "Expected successful compilation, got errors: " ++ show errors
                Right _ -> return ()
        it "Should error when function parameter is used in refined struct (cannot verify at compile time)" $ do
            result <-
                compileWithErrors
                    [r|
                struct Age = (value: Int) satisfies (value >= 0)
                let createAge (x: Int) : Age = Age{value: x}
                let main : IO<Unit> = do
                    println (createAge 14)
                end
                |]
            case result of
                Left errors -> do
                    let errorMessages = map BytecodeCompiler.errorMessage errors
                    any ("Cannot verify refinement" `isInfixOf`) errorMessages
                        `shouldBe` True
                Right _ -> expectationFailure "Expected compilation error when function parameter used in refined struct"
    describe "Generics" $ do
        it "Should compile a simple generic function" $
            compile
                [r|
                let id<T> (x: T) : T = x
                let main : IO<Unit> = unsafePrint (id 42)
                |]
                `shouldReturn` [Label "id#0", StoreSideStack, LStore "x", LLoad "x", ClearSideStack, Ret, Label "main", StoreSideStack, Push 42, Call "id#0", Builtin Print, ClearSideStack, Push $ DInt 0, Exit]
        it "Should compile generic function with trait constraint" $
            compile
                [r|
                trait Number
                impl Number for Int
                impl Number for Float
                let add<N: Number> (a: N b: N) : N = a + b
                let main : IO<Unit> = unsafePrint (add 1, 2)
                |]
                `shouldReturn` [Label "add#0", StoreSideStack, LStore "b", LStore "a", LLoad "a", LStore "__op_a_1", LLoad "b", LStore "__op_b_1", LLoad "__op_a_1", LLoad "__op_b_1", Call "+", ClearSideStack, Ret, Label "main", StoreSideStack, Push 1, Push 2, Call "add#0", Builtin Print, ClearSideStack, Push $ DInt 0, Exit]
        it "Should require matching generic types" $ do
            let prog =
                    [r|
                trait Number
                impl Number for Int
                impl Number for Float
                let add<N: Number> (a: N b: N) : N = a + b
                let main : IO<Unit> = unsafePrint (add 1, 2.0)
                |]
            compile prog `shouldThrow` (const True :: SomeException -> Bool)
        it "Should compile generic trait with type parameters" $ do
            result <-
                compileWithErrors
                    [r|
                trait Monad<T> = do
                    bind :: Self -> (Any -> Self) -> Self
                end
                struct Optional = (value: Any)
                impl Monad<Optional> for Optional = do
                    bind Optional{value: x} f = f x
                end
                let main : IO<Unit> = unsafePrint 42
                |]
            case result of
                Left errors -> expectationFailure $ "Expected successful compilation, got: " ++ show errors
                Right _ -> return ()
        it "Should error when impl type argument count mismatches trait generics" $ do
            result <-
                compileWithErrors
                    [r|
                trait Monad<T> = do
                    bind :: Self -> (Any -> Self) -> Self
                end
                struct Optional = (value: Any)
                impl Monad<Optional, Int> for Optional = do
                    bind Optional{value: x} f = f x
                end
                let main : IO<Unit> = unsafePrint 42
                |]
            case result of
                Left errors -> do
                    let errorMessages = map (\(BytecodeCompiler.CompilerError msg _ _) -> msg) errors
                    any ("type argument count mismatch" `isInfixOf`) errorMessages
                        `shouldBe` True
                Right _ -> expectationFailure $ "Expected compilation error for type argument count mismatch, but got: " ++ show result
        it "Should error when impl method not in trait" $ do
            result <-
                compileWithErrors
                    [r|
                trait Monad<T> = do
                    bind :: Self -> (Any -> Self) -> Self
                end
                struct Optional = (value: Any)
                impl Monad<Optional> for Optional = do
                    bind Optional{value: x} f = f x
                    return x = Optional{value: x}
                end
                let main : IO<Unit> = unsafePrint 42
                |]
            case result of
                Left errors -> do
                    let errorMessages = map BytecodeCompiler.errorMessage errors
                    any ("Method 'return' is not declared in trait 'Monad'" `isInfixOf`) errorMessages
                        `shouldBe` True
                Right _ -> expectationFailure "Expected compilation error for method not in trait"
        describe "Type parameter validation" $ do
            it "Should error on invalid type parameter in struct is clause" $ do
                result <-
                    compileWithErrors
                        [r|
                    trait Optional<T>
                    struct None = () is Optional<T>
                    let main : IO<Unit> = unsafePrint 42
                    |]
                case result of
                    Left errors -> do
                        let errorMessages = map BytecodeCompiler.errorMessage errors
                        any ("Invalid type parameter" `isInfixOf`) errorMessages
                            `shouldBe` True
                        any ("struct None" `isInfixOf`) errorMessages
                            `shouldBe` True
                    Right _ -> expectationFailure "Expected compilation error for invalid type parameter in is clause"
            it "Should allow valid type parameter in struct is clause" $ do
                result <-
                    compileWithErrors
                        [r|
                    trait Optional<T>
                    struct Some<T> = (value: T) is Optional<T>
                    let main : IO<Unit> = unsafePrint 42
                    |]
                case result of
                    Left errors -> expectationFailure $ "Expected successful compilation, got: " ++ show errors
                    Right _ -> return ()
            it "Should error on invalid type parameter in struct literal" $ do
                result <-
                    compileWithErrors
                        [r|
                    struct Some<T> = (value: T)
                    let main : IO<Unit> = do
                        let x = Some<U>{value: 42}
                        unsafePrint x
                    end
                    |]
                case result of
                    Left errors -> do
                        let errorMessages = map BytecodeCompiler.errorMessage errors
                        any ("Invalid type parameter" `isInfixOf`) errorMessages
                            `shouldBe` True
                        any ("struct literal Some" `isInfixOf`) errorMessages
                            `shouldBe` True
                    Right _ -> expectationFailure "Expected compilation error for invalid type parameter in struct literal"
            it "Should allow valid type parameter in struct literal" $ do
                result <-
                    compileWithErrors
                        [r|
                    struct Some<T> = (value: T)
                    let main : IO<Unit> = do
                        let x = Some<Int>{value: 42}
                        unsafePrint x
                    end
                    |]
                case result of
                    Left errors -> expectationFailure $ "Expected successful compilation, got: " ++ show errors
                    Right _ -> return ()
            it "Should error on invalid type parameter in impl statement" $ do
                result <-
                    compileWithErrors
                        [r|
                    trait Monad<T> = do
                        bind :: Self -> (Any -> Self) -> Self
                    end
                    struct Optional = (value: Any)
                    impl Monad<T> for Optional = do
                        bind x f = f x
                    end
                    let main : IO<Unit> = unsafePrint 42
                    |]
                case result of
                    Left errors -> do
                        let errorMessages = map BytecodeCompiler.errorMessage errors
                        any ("Invalid type parameter" `isInfixOf`) errorMessages
                            `shouldBe` True
                        any ("impl Monad" `isInfixOf`) errorMessages
                            `shouldBe` True
                    Right _ -> expectationFailure "Expected compilation error for invalid type parameter in impl"
            it "Should allow valid type parameter in impl statement" $ do
                result <-
                    compileWithErrors
                        [r|
                    trait Monad<T> = do
                        bind :: Self -> (Any -> Self) -> Self
                    end
                    struct Optional = (value: Any)
                    impl Monad<Optional> for Optional = do
                        bind x f = f x
                    end
                    let main : IO<Unit> = unsafePrint 42
                    |]
                case result of
                    Left errors -> expectationFailure $ "Expected successful compilation, got: " ++ show errors
                    Right _ -> return ()
            it "Should allow valid type parameter in function declaration" $ do
                result <-
                    compileWithErrors
                        [r|
                    struct Some<T> = (value: T)
                    let test<U> (x: Some<U>): Some<U> = x
                    let main : IO<Unit> = do
                        let x = Some<Int>{value: 42}
                        unsafePrint x
                    end
                    |]
                case result of
                    Left errors -> expectationFailure $ "Expected successful compilation, got: " ++ show errors
                    Right _ -> return ()
            it "Should error on nested invalid type parameter" $ do
                result <-
                    compileWithErrors
                        [r|
                    trait Optional<T>
                    struct None = () is Optional<List<T>>
                    let main : IO<Unit> = unsafePrint 42
                    |]
                case result of
                    Left errors -> do
                        let errorMessages = map BytecodeCompiler.errorMessage errors
                        any ("Invalid type parameter" `isInfixOf`) errorMessages
                            `shouldBe` True
                    Right _ -> expectationFailure "Expected compilation error for nested invalid type parameter"
            it "Should allow nested valid type parameter" $ do
                result <-
                    compileWithErrors
                        [r|
                    trait Optional<T>
                    struct None = () is Optional<List<Int>>
                    let main : IO<Unit> = unsafePrint 42
                    |]
                case result of
                    Left errors -> expectationFailure $ "Expected successful compilation, got: " ++ show errors
                    Right _ -> return ()
        it "Should compile generic function with struct types" $ do
            result <-
                compile
                    [r|
                struct Point = (x: Int, y: Int)
                let getX<T> (p: T) : Int = p.x
                let main : IO<Unit> = unsafePrint (getX Point { x: 1, y: 2 })
                |]
            result `shouldSatisfy` any (\case Label name -> "getX" `isInfixOf` name; _ -> False)
            result `shouldSatisfy` any (\case Call name -> "getX" `isInfixOf` name; _ -> False)
        it "Should handle generic return types" $
            compile
                [r|
                let identity<T> (x: T) : T = x
                let main : IO<Unit> = unsafePrint (identity "hello")
                |]
                `shouldReturn` [Label "identity#0", StoreSideStack, LStore "x", LLoad "x", ClearSideStack, Ret, Label "main", StoreSideStack, Push $ DString "hello", Call "identity#0", Builtin Print, ClearSideStack, Push $ DInt 0, Exit]
    describe "Function resolution" $ do
        it "Should find function in context when it matches a parameter name" $ do
            result <-
                compileWithErrors
                    [r|
                let outer (f: Any) : Int = f 42
                let main : IO<Unit> = unsafePrint (outer (\x -> x + 1))
                |]
            case result of
                Left errors -> expectationFailure $ "Expected successful compilation, got errors: " ++ show errors
                Right _ -> return ()
        it "Should find function when it is declared via FuncDec" $ do
            result <-
                compileWithErrors
                    [r|
                myFunc :: Int -> Int
                myFunc (x: Int) : Int = x + 1
                let main : IO<Unit> = unsafePrint (myFunc 5)
                |]
            case result of
                Left errors -> expectationFailure $ "Expected successful compilation, got errors: " ++ show errors
                Right _ -> return ()
        it "Should error when function is not found" $ do
            result <-
                compileWithErrors
                    [r|
                let main : IO<Unit> = unsafePrint (undefinedFunc 42)
                |]
            case result of
                Left errors -> do
                    let errorMessages = map BytecodeCompiler.errorMessage errors
                    any ("Function undefinedFunc not found." `isInfixOf`) errorMessages
                        `shouldBe` True
                Right _ -> expectationFailure "Expected compilation error for undefined function"
    describe "Return type checking" $ do
        it "Should compile when return type matches declaration" $ do
            result <-
                compileWithErrors
                    [r|
                let add (x: Int) : Int = x + 1
                let main : IO<Unit> = unsafePrint (add 5)
                |]
            case result of
                Left errors -> expectationFailure $ "Expected successful compilation, got: " ++ show errors
                Right _ -> return ()
        it "Should error when return type mismatches declaration" $ do
            result <-
                compileWithErrors
                    [r|
                let wrong (x: Int) : Int = "hello"
                let main : IO<Unit> = unsafePrint (wrong 5)
                |]
            case result of
                Left errors -> do
                    let errorMessages = map BytecodeCompiler.errorMessage errors
                    any ("Return type mismatch" `isInfixOf`) errorMessages
                        `shouldBe` True
                Right _ -> expectationFailure "Expected return type mismatch error"
        it "Should pass with compatible numeric types" $ do
            result <-
                compileWithErrors
                    [r|
                let toFloat (x: Int) : Float = x as Float
                let main : IO<Unit> = unsafePrint (toFloat 5)
                |]
            case result of
                Left errors -> expectationFailure $ "Expected successful compilation, got: " ++ show errors
                Right _ -> return ()
        it "Should pass when return type is Any" $ do
            result <-
                compileWithErrors
                    [r|
                let identity (x: Int) : Any = x
                let main : IO<Unit> = unsafePrint (identity 5)
                |]
            case result of
                Left errors -> expectationFailure $ "Expected successful compilation, got: " ++ show errors
                Right _ -> return ()
