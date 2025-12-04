module ParserSpec (spec) where

import AST
import GHC.Generics
import Parser
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Text.RawString.QQ (r)

instance Arbitrary Type where
    arbitrary = sized $ \n ->
        if n <= 0
            then oneof [return Any, return Unknown, return None, return Self, StructT <$> elements ["Int", "String", "Bool", "Float", "Char"] <*> return []]
            else
                oneof
                    [ return Any
                    , return Unknown
                    , return None
                    , return Self
                    , StructT <$> elements ["Int", "String", "Bool", "Float", "Char"] <*> return []
                    , List <$> resize (n `div` 2) arbitrary
                    , do
                        size <- choose (2, min 4 (n `div` 2))
                        Tuple <$> vectorOf size (resize (n `div` 2) arbitrary)
                    , do
                        argCount <- choose (0, min 2 (n `div` 2))
                        args <- vectorOf argCount (resize (n `div` 2) arbitrary)
                        ret <- resize (n `div` 2) arbitrary
                        return $ AST.Fn args ret
                    ]
    shrink = genericShrink

isFn :: Type -> Bool
isFn (AST.Fn _ _) = True
isFn _ = False

parserCompilerFlags :: CompilerFlags
parserCompilerFlags = initCompilerFlags{needsMain = False}

spec :: Spec
spec = do
    describe "Binary operations" $ do
        it "Can parse free operators" $
            parseProgram "x := 2" parserCompilerFlags
                `shouldBe` Right
                    (Program [FuncCall{funcName = ":=", funcArgs = [Var{varName = "x", varPos = Position (0, 2)}, IntLit{intValue = 2, intPos = anyPosition}], funcPos = anyPosition}] Nothing)
    describe "Function definitions" $ do
        it "Should parse a simple function" $
            parseProgram "add a b = a + b" parserCompilerFlags
                `shouldBe` Right
                    (Program [FuncDef{name = "add", args = [Var{varName = "a", varPos = anyPosition}, Var{varName = "b", varPos = anyPosition}], body = Add{addLhs = Var{varName = "a", varPos = anyPosition}, addRhs = Var{varName = "b", varPos = anyPosition}, addPos = anyPosition}, funcDefPos = anyPosition}] Nothing)
        it "Should prase a function with 0 parameters" $
            parseProgram "thunk = 1" parserCompilerFlags
                `shouldBe` Right
                    (Program [FuncDef{name = "thunk", args = [], body = IntLit{intValue = 1, intPos = anyPosition}, funcDefPos = anyPosition}] Nothing)
    describe "compareTypes" $ do
        it "Should be true for exact matches" $
            property $
                \t -> compareTypes t t `shouldBe` True
        xit "Should be false for non-exact matches (except for Any or Fn or Self)" $
            property $
                \t1 t2 ->
                    -- Janky
                    notElem Any [t1, t2]
                        && not (isFn t1 || isFn t2)
                        && notElem Self [t1, t2]
                        && notElem (List Self) [t1, t2]
                        && t1
                            /= t2
                        ==> compareTypes t1 t2
                            `shouldBe` False
        it "Any type should be a subtype of Any" $
            property $
                \t -> compareTypes t Any `shouldBe` True
        it "Fn any any should be a subtype of Fn Any Any" $
            property $
                \t -> compareTypes (AST.Fn [t] Any) (AST.Fn [Any] Any) `shouldBe` True
        it "List types should be equal if their element types are equal" $
            property $
                \t -> compareTypes (List t) (List t) `shouldBe` True
        it "StructT types should be equal if their fields are equal" $
            property $
                \t -> compareTypes (StructT t []) (StructT t []) `shouldBe` True
        it "Tuple types should be equal if their element types are equal" $
            compareTypes (Tuple [StructT "Int" [], StructT "String" []]) (Tuple [StructT "Int" [], StructT "String" []]) `shouldBe` True
        it "Tuple types should not be equal if lengths differ" $
            compareTypes (Tuple [StructT "Int" []]) (Tuple [StructT "Int" [], StructT "String" []]) `shouldBe` False
        it "Tuple types should not be equal if element types differ" $
            compareTypes (Tuple [StructT "Int" [], StructT "String" []]) (Tuple [StructT "String" [], StructT "Int" []]) `shouldBe` False
    describe "Basic" $ do
        it "Should parse a simple program" $
            parseProgram "let main : IO = print \"Hello, world!\"" parserCompilerFlags
                `shouldBe` Right
                    (Program [Function{def = [FuncDef{name = "main", args = [], body = FuncCall{funcName = "print", funcArgs = [StringLit{stringValue = "Hello, world!", stringPos = anyPosition}], funcPos = anyPosition}, funcDefPos = anyPosition}], dec = FuncDec{name = "main", types = [StructT "IO" []], generics = [], funcDecPos = anyPosition}, functionPos = anyPosition}] Nothing)
    describe "Struct" $ do
        it "Member access" $ do
            parseProgram "bello{}.name" parserCompilerFlags
                `shouldBe` Right
                    (Program [StructAccess{structAccessStruct = StructLit{structLitName = "bello", structLitFields = [], structLitTypeArgs = [], structLitPos = anyPosition}, structAccessField = Var{varName = "name", varPos = anyPosition}, structAccessPos = anyPosition}] Nothing)
        it "Should parse generic struct" $ do
            parseProgram "struct Example<N: Number> = (content: N)" parserCompilerFlags
                `shouldBe` Right
                    (Program [Struct{name = "Example", fields = [("content", StructT "N" [])], refinement = Nothing, refinementSrc = "", is = [], isValueStruct = False, generics = [GenericExpr "N" (Just $ StructT "Number" [])], structPos = anyPosition}] Nothing)
        it "Should parse struct literal with type arguments" $ do
            parseProgram "Example<Int>{content: 42}" parserCompilerFlags
                `shouldBe` Right
                    (Program [StructLit{structLitName = "Example", structLitFields = [("content", IntLit{intValue = 42, intPos = anyPosition})], structLitTypeArgs = [StructT "Int" []], structLitPos = anyPosition}] Nothing)
        it "Can define" $
            parseProgram "struct Teacher = ()" parserCompilerFlags
                `shouldBe` Right (Program [Struct{name = "Teacher", fields = [], refinement = Nothing, refinementSrc = "", is = [], isValueStruct = False, generics = [], structPos = anyPosition}] Nothing)
        it "Can define with is" $ do
            parseProgram "struct Teacher = () is Person" parserCompilerFlags
                `shouldBe` Right (Program [Struct{name = "Teacher", fields = [], refinement = Nothing, refinementSrc = "", is = [StructT "Person" []], isValueStruct = False, generics = [], structPos = anyPosition}] Nothing)
        it "Can define with multiple is" $ do
            parseProgram "struct Teacher = () is Person, Employee" parserCompilerFlags
                `shouldBe` Right (Program [Struct{name = "Teacher", fields = [], refinement = Nothing, refinementSrc = "", is = [StructT "Person" [], StructT "Employee" []], isValueStruct = False, generics = [], structPos = anyPosition}] Nothing)
        it "Should parse generic struct" $ do
            parseProgram "struct Example<N: Number> = (content: N)" parserCompilerFlags
                `shouldBe` Right
                    (Program [Struct{name = "Example", fields = [("content", StructT "N" [])], refinement = Nothing, refinementSrc = "", is = [], isValueStruct = False, generics = [GenericExpr "N" (Just $ StructT "Number" [])], structPos = anyPosition}] Nothing)
        it "Should parse struct literal with type arguments" $ do
            parseProgram "Example<Int>{content: 42}" parserCompilerFlags
                `shouldBe` Right
                    (Program [StructLit{structLitName = "Example", structLitFields = [("content", IntLit{intValue = 42, intPos = anyPosition})], structLitTypeArgs = [StructT "Int" []], structLitPos = anyPosition}] Nothing)
    describe "Value structs" $ do
        it "Can define value struct without refinement" $ do
            parseProgram "value struct PositiveInt = (num: Int)" parserCompilerFlags
                `shouldBe` Right (Program [Struct{name = "PositiveInt", fields = [("num", StructT "Int" [])], refinement = Nothing, refinementSrc = "", is = [], isValueStruct = True, generics = [], structPos = anyPosition}] Nothing)
        it "Can define value struct with refinement" $ do
            let result = parseProgram "value struct EvenNumber = (num: Int) satisfies ((num % 2) == 0)" parserCompilerFlags
            case result of
                Right (Program [Struct{name = "EvenNumber", fields = [("num", StructT "Int" [])], refinement = Just (Eq (Modulo (Var "num" _) (IntLit 2 _) _) (IntLit 0 _) _), isValueStruct = True, generics = [], structPos = _}] _) -> return ()
                _ -> expectationFailure $ "Failed to parse value struct with refinement: " ++ show result
        it "Can define value struct with is clause" $ do
            parseProgram "value struct PositiveInt = (num: Int) is Printable" parserCompilerFlags
                `shouldBe` Right (Program [Struct{name = "PositiveInt", fields = [("num", StructT "Int" [])], refinement = Nothing, refinementSrc = "", is = [StructT "Printable" []], isValueStruct = True, generics = [], structPos = anyPosition}] Nothing)
    describe "Traits" $ do
        it "Should parse generic trait" $ do
            parseProgram "trait Monad<T> = do\n  bind :: Self -> (Any -> Self) -> Self\nend" parserCompilerFlags
                `shouldBe` Right
                    (Program [Trait{name = "Monad", methods = [FuncDec{name = "bind", types = [Self, AST.Fn [Any] Self, Self], generics = [], funcDecPos = anyPosition}], generics = [GenericExpr "T" Nothing], requiredProperties = [], refinement = Nothing, refinementSrc = "", traitPos = anyPosition}] Nothing)
        it "Should parse a trait decleration" $
            parseProgram "trait Show = do\nshow :: Self -> String\nend" parserCompilerFlags
                `shouldBe` Right
                    (Program [Trait{name = "Show", methods = [FuncDec{name = "show", types = [Self, StructT "String" []], generics = [], funcDecPos = anyPosition}], generics = [], requiredProperties = [], refinement = Nothing, refinementSrc = "", traitPos = anyPosition}] Nothing)
        it "Should parse a trait declaration with multiple methods" $
            parseProgram "trait Show = do\nshow :: Self -> String\nshow2 :: Self -> String\nend" parserCompilerFlags
                `shouldBe` Right
                    (Program [Trait{name = "Show", methods = [FuncDec{name = "show", types = [Self, StructT "String" []], generics = [], funcDecPos = anyPosition}, FuncDec{name = "show2", types = [Self, StructT "String" []], generics = [], funcDecPos = anyPosition}], generics = [], requiredProperties = [], refinement = Nothing, refinementSrc = "", traitPos = anyPosition}] Nothing)
        it "Should parse a trait implementation" $
            parseProgram "impl Show for Point = do\nshow point = \"Point {x: \" : show point.x : \", y: \" : show point.y : \"}\"\nend" parserCompilerFlags
                `shouldBe` Right
                    ( Program
                        [ Impl
                            { trait = "Show"
                            , traitTypeArgs = []
                            , for = StructT "Point" []
                            , methods =
                                [ FuncDef{name = "show", args = [Var{varName = "point", varPos = anyPosition}], body = parseFreeUnsafe "\"Point {x: \" : show point.x : \", y: \" : show point.y : \"}\"", funcDefPos = anyPosition}
                                ]
                            , implPos = anyPosition
                            }
                        ]
                        Nothing
                    )
        it "Should parse impl with type parameters" $ do
            parseProgram "trait Monad<T> = do\n  bind :: Self -> (Any -> Self) -> Self\nend\nimpl Monad<Optional> for Optional = do\n  bind x f = f x\nend" parserCompilerFlags
                `shouldSatisfy` \case
                    Right (Program [Trait{name = "Monad", generics = [GenericExpr "T" Nothing]}, Impl{trait = "Monad", traitTypeArgs = [StructT "Optional" []]}] _) -> True
                    _ -> False
        it "Should parse trait with multiple type parameters" $ do
            parseProgram "trait Functor<F, A> = do\n  map :: Self -> (A -> A) -> Self\nend" parserCompilerFlags
                `shouldSatisfy` \case
                    Right (Program [Trait{name = "Functor", generics = [GenericExpr "F" Nothing, GenericExpr "A" Nothing]}] Nothing) -> True
                    _ -> False
        it "Should parse trait with constrained type parameters" $ do
            parseProgram "trait Number\nimpl Number for Int\nimpl Number for Float\ntrait Container<T: Number> = do\n  get :: Self -> T\nend" parserCompilerFlags
                `shouldSatisfy` \case
                    Right (Program [Trait{name = "Number"}, Impl{}, Impl{}, Trait{name = "Container", generics = [GenericExpr "T" (Just (StructT "Number" []))]}] _) -> True
                    _ -> False
        it "Should parse impl with multiple type arguments" $ do
            parseProgram "trait Pair<T, U> = do\n  first :: Self -> T\n  second :: Self -> U\nend\nstruct IntStringPair = (first: Int, second: String)\nimpl Pair<Int, String> for IntStringPair = do\n  first self = self.first\n  second self = self.second\nend" parserCompilerFlags
                `shouldSatisfy` \case
                    Right (Program [Trait{name = "Pair", generics = [GenericExpr "T" Nothing, GenericExpr "U" Nothing]}, Struct{}, Impl{trait = "Pair", traitTypeArgs = [StructT "Int" [], StructT "String" []]}] _) -> True
                    _ -> False
        it "Should parse impl with generic struct type argument" $ do
            parseProgram "trait Monad<T> = do\n  bind :: Self -> (Any -> Self) -> Self\nend\nstruct Option<T> = (value: T)\nstruct Optional = (value: Any)\nimpl Monad<Option<Int>> for Optional = do\n  bind x f = f x.value\nend" parserCompilerFlags
                `shouldSatisfy` \case
                    Right (Program [Trait{name = "Monad"}, Struct{name = "Option", generics = [_]}, Struct{name = "Optional"}, Impl{trait = "Monad", traitTypeArgs = [StructT "Option" [StructT "Int" []]], for = StructT "Optional" []}] _) -> True
                    _ -> False
        it "Should parse trait with required properties" $ do
            parseProgram "trait Printable = (name: String, age: Int) do\n  print :: Self -> String\nend" parserCompilerFlags
                `shouldSatisfy` \case
                    Right (Program [Trait{name = "Printable", requiredProperties = [("name", StructT "String" []), ("age", StructT "Int" [])]}] _) -> True
                    _ -> False
        it "Should parse trait with refinement" $ do
            parseProgram "trait PositiveNumber satisfies (x > 0) = do\n  add :: Self -> Self -> Self\nend" parserCompilerFlags
                `shouldSatisfy` \case
                    Right (Program [Trait{name = "PositiveNumber", refinement = Just _}] _) -> True
                    _ -> False
        it "Should parse trait with both required properties and refinement" $ do
            parseProgram "trait ValidPerson = (name: String, age: Int) satisfies (age > 0) do\n  getName :: Self -> String\nend" parserCompilerFlags
                `shouldSatisfy` \case
                    Right (Program [Trait{name = "ValidPerson", requiredProperties = [("name", StructT "String" []), ("age", StructT "Int" [])], refinement = Just _}] _) -> True
                    _ -> False
        it "Should parse trait with generics and required properties" $ do
            parseProgram "trait Container<T> = (value: T) do\n  get :: Self -> T\nend" parserCompilerFlags
                `shouldSatisfy` \case
                    Right (Program [Trait{name = "Container", generics = [GenericExpr "T" Nothing], requiredProperties = [("value", StructT "T" [])]}] _) -> True
                    _ -> False
    describe "Gravis" $ do
        it "Should parse operators escaped using gravis in dec correctly" $ do
            parseProgram
                [r|
                + :: Any -> Any -> Any
                `-` :: Any -> Any -> Any
                * :: Any -> Any -> Any
                / :: Any -> Any -> Any
                == :: Any -> Any -> Any
                |]
                parserCompilerFlags
                `shouldBe` Right
                    (Program [FuncDec{name = "+", types = [Any, Any, Any], generics = [], funcDecPos = anyPosition}, FuncDec{name = "-", types = [Any, Any, Any], generics = [], funcDecPos = anyPosition}, FuncDec{name = "*", types = [Any, Any, Any], generics = [], funcDecPos = anyPosition}, FuncDec{name = "/", types = [Any, Any, Any], generics = [], funcDecPos = anyPosition}, FuncDec{name = "==", types = [Any, Any, Any], generics = [], funcDecPos = anyPosition}] Nothing)
        it "Should be able to use gravis escaped functions in calls" $
            do
                parseProgram
                    [r|
                filter (`==`1), [1, 2, 3]
            |]
                    parserCompilerFlags
                `shouldBe` Right
                    (Program [FuncCall{funcName = "filter", funcArgs = [FuncCall{funcName = "==", funcArgs = [IntLit{intValue = 1, intPos = anyPosition}], funcPos = anyPosition}, ListLit{listLitExprs = [IntLit{intValue = 1, intPos = anyPosition}, IntLit{intValue = 2, intPos = anyPosition}, IntLit{intValue = 3, intPos = anyPosition}], listLitPos = anyPosition}], funcPos = anyPosition}] Nothing)
    describe "Unary minus" $ do
        it "Should parse unary minus" $
            parseProgram "(-1)" parserCompilerFlags
                `shouldBe` Right
                    (Program [UnaryMinus{unaryMinusExpr = IntLit{intValue = 1, intPos = anyPosition}, unaryMinusPos = anyPosition}] Nothing)
        it "Should be able to use negative numbers in multiplication" $
            parseProgram "1 * (-1)" parserCompilerFlags
                `shouldBe` Right
                    (Program [Mul{mulLhs = IntLit{intValue = 1, intPos = anyPosition}, mulRhs = UnaryMinus{unaryMinusExpr = IntLit{intValue = 1, intPos = anyPosition}, unaryMinusPos = anyPosition}, mulPos = anyPosition}] Nothing)
        it "Should be able to negate expressions in parentheses" $
            parseProgram "2 * (-(3-x*5))" parserCompilerFlags
                `shouldBe` Right
                    (Program [Mul{mulLhs = IntLit{intValue = 2, intPos = anyPosition}, mulRhs = UnaryMinus{unaryMinusExpr = Sub{subLhs = IntLit{intValue = 3, intPos = anyPosition}, subRhs = Mul{mulLhs = Var{varName = "x", varPos = anyPosition}, mulRhs = IntLit{intValue = 5, intPos = anyPosition}, mulPos = anyPosition}, subPos = anyPosition}, unaryMinusPos = anyPosition}, mulPos = anyPosition}] Nothing)
    describe "Parentheses" $ do
        it "Should parse parentheses" $
            parseProgram "(1 + 2) * 3" parserCompilerFlags
                `shouldBe` Right
                    (Program [Mul{mulLhs = Add{addLhs = IntLit{intValue = 1, intPos = anyPosition}, addRhs = IntLit{intValue = 2, intPos = anyPosition}, addPos = anyPosition}, mulRhs = IntLit{intValue = 3, intPos = anyPosition}, mulPos = anyPosition}] Nothing)
        it "Should parse parenthesis application correctly" $
            parseProgram "(x y) z" parserCompilerFlags
                `shouldBe` Right
                    (Program [ParenApply{parenApplyExpr = FuncCall{funcName = "x", funcArgs = [Var{varName = "y", varPos = anyPosition}], funcPos = anyPosition}, parenApplyArgs = [Var{varName = "z", varPos = anyPosition}], parenApplyPos = anyPosition}] Nothing)
        it "bottles (i)-1" $
            parseProgram "bottles (i)-1" parserCompilerFlags
                `shouldBe` Right
                    (Program [FuncCall{funcName = "bottles", funcArgs = [Sub{subLhs = Var{varName = "i", varPos = anyPosition}, subRhs = IntLit{intValue = 1, intPos = anyPosition}, subPos = anyPosition}], funcPos = anyPosition}] Nothing)

    describe "Generics" $ do
        it "Should parse let generics" $ do
            parseProgram
                [r|
                let add<N: Number> (a: N b: N) : N = do
                  a + b
                end
            |]
                parserCompilerFlags
                `shouldBe` Right
                    (Program [Function{def = [FuncDef{name = "add", args = [Var{varName = "a", varPos = anyPosition}, Var{varName = "b", varPos = anyPosition}], body = DoBlock{doBlockExprs = [Add{addLhs = Var{varName = "a", varPos = anyPosition}, addRhs = Var{varName = "b", varPos = anyPosition}, addPos = anyPosition}], doBlockPos = anyPosition}, funcDefPos = anyPosition}], dec = FuncDec{name = "add", types = [StructT "N" [], StructT "N" [], StructT "N" []], generics = [GenericExpr "N" (Just $ StructT "Number" [])], funcDecPos = anyPosition}, functionPos = anyPosition}] Nothing)
        it "Should parse classic decleration generics" $ do
            parseProgram
                [r|
                add<N: Number> :: N -> N -> N
            |]
                parserCompilerFlags
                `shouldBe` Right
                    (Program [FuncDec{name = "add", types = [StructT "N" [], StructT "N" [], StructT "N" []], generics = [GenericExpr "N" (Just $ StructT "Number" [])], funcDecPos = anyPosition}] Nothing)
    describe "Binary Operators" $ do
        it "Should parse addition" $
            parseProgram "1 + 2" parserCompilerFlags
                `shouldBe` Right
                    (Program [Add{addLhs = IntLit{intValue = 1, intPos = anyPosition}, addRhs = IntLit{intValue = 2, intPos = anyPosition}, addPos = anyPosition}] Nothing)
        it "Should parse subtraction" $
            parseProgram "3 - 5" parserCompilerFlags
                `shouldBe` Right
                    (Program [Sub{subLhs = IntLit{intValue = 3, intPos = anyPosition}, subRhs = IntLit{intValue = 5, intPos = anyPosition}, subPos = anyPosition}] Nothing)
        it "Should parse multiplication" $
            parseProgram "8 * 4" parserCompilerFlags
                `shouldBe` Right
                    (Program [Mul{mulLhs = IntLit{intValue = 8, intPos = anyPosition}, mulRhs = IntLit{intValue = 4, intPos = anyPosition}, mulPos = anyPosition}] Nothing)
        it "Should parse division" $
            parseProgram "10 / 2" parserCompilerFlags
                `shouldBe` Right
                    (Program [Div{divLhs = IntLit{intValue = 10, intPos = anyPosition}, divRhs = IntLit{intValue = 2, intPos = anyPosition}, divPos = anyPosition}] Nothing)
        it "Should parse modulo" $
            parseProgram "11 % 3" parserCompilerFlags
                `shouldBe` Right
                    (Program [Modulo{moduloLhs = IntLit{intValue = 11, intPos = anyPosition}, moduloRhs = IntLit{intValue = 3, intPos = anyPosition}, moduloPos = anyPosition}] Nothing)
        it "Should parse chained operations with correct precedence (1 + 2 * 3)" $
            parseProgram "1 + 2 * 3" parserCompilerFlags
                `shouldBe` Right
                    (Program [Add{addLhs = IntLit{intValue = 1, intPos = anyPosition}, addRhs = Mul{mulLhs = IntLit{intValue = 2, intPos = anyPosition}, mulRhs = IntLit{intValue = 3, intPos = anyPosition}, mulPos = anyPosition}, addPos = anyPosition}] Nothing)
        it "Should parse chained operations with correct precedence ((1 + 2) * 3)" $
            parseProgram "(1 + 2) * 3" parserCompilerFlags
                `shouldBe` Right
                    (Program [Mul{mulLhs = Add{addLhs = IntLit{intValue = 1, intPos = anyPosition}, addRhs = IntLit{intValue = 2, intPos = anyPosition}, addPos = anyPosition}, mulRhs = IntLit{intValue = 3, intPos = anyPosition}, mulPos = anyPosition}] Nothing)
        it "Should parse equality" $
            parseProgram "4 == 5" parserCompilerFlags
                `shouldBe` Right
                    (Program [Eq{eqLhs = IntLit{intValue = 4, intPos = anyPosition}, eqRhs = IntLit{intValue = 5, intPos = anyPosition}, eqPos = anyPosition}] Nothing)
        it "Should parse inequality" $
            parseProgram "4 != 5" parserCompilerFlags
                `shouldBe` Right
                    (Program [Neq{neqLhs = IntLit{intValue = 4, intPos = anyPosition}, neqRhs = IntLit{intValue = 5, intPos = anyPosition}, neqPos = anyPosition}] Nothing)
        it "Should parse less than" $
            parseProgram "6 < 7" parserCompilerFlags
                `shouldBe` Right
                    (Program [Lt{ltLhs = IntLit{intValue = 6, intPos = anyPosition}, ltRhs = IntLit{intValue = 7, intPos = anyPosition}, ltPos = anyPosition}] Nothing)
        it "Should parse greater than" $
            parseProgram "8 > 3" parserCompilerFlags
                `shouldBe` Right
                    (Program [Gt{gtLhs = IntLit{intValue = 8, intPos = anyPosition}, gtRhs = IntLit{intValue = 3, intPos = anyPosition}, gtPos = anyPosition}] Nothing)
        it "Should parse less-than or equal" $
            parseProgram "2 <= 5" parserCompilerFlags
                `shouldBe` Right
                    (Program [Le{leLhs = IntLit{intValue = 2, intPos = anyPosition}, leRhs = IntLit{intValue = 5, intPos = anyPosition}, lePos = anyPosition}] Nothing)
        it "Should parse greater-than or equal" $
            parseProgram "10 >= 10" parserCompilerFlags
                `shouldBe` Right
                    (Program [Ge{geLhs = IntLit{intValue = 10, intPos = anyPosition}, geRhs = IntLit{intValue = 10, intPos = anyPosition}, gePos = anyPosition}] Nothing)
        it "Should parse and" $
            parseProgram "True && False" parserCompilerFlags
                `shouldBe` Right
                    (Program [And{andLhs = BoolLit{boolValue = True, boolPos = anyPosition}, andRhs = BoolLit{boolValue = False, boolPos = anyPosition}, andPos = anyPosition}] Nothing)
        it "Should parse or" $
            parseProgram "x || y" parserCompilerFlags
                `shouldBe` Right
                    (Program [Or{orLhs = Var{varName = "x", varPos = anyPosition}, orRhs = Var{varName = "y", varPos = anyPosition}, orPos = anyPosition}] Nothing)
    describe "Tuples" $ do
        it "Should parse tuple literal with 2 elements" $
            parseProgram "(1, 2)" parserCompilerFlags
                `shouldBe` Right
                    (Program [TupleLit{tupleLitExprs = [IntLit{intValue = 1, intPos = anyPosition}, IntLit{intValue = 2, intPos = anyPosition}], tupleLitPos = anyPosition}] Nothing)
        it "Should parse tuple literal with 3 elements" $
            parseProgram "(1, \"hello\", True)" parserCompilerFlags
                `shouldBe` Right
                    (Program [TupleLit{tupleLitExprs = [IntLit{intValue = 1, intPos = anyPosition}, StringLit{stringValue = "hello", stringPos = anyPosition}, BoolLit{boolValue = True, boolPos = anyPosition}], tupleLitPos = anyPosition}] Nothing)
        it "Should parse tuple literal with 1 element using trailing comma" $
            parseProgram "(1,)" parserCompilerFlags
                `shouldBe` Right
                    (Program [TupleLit{tupleLitExprs = [IntLit{intValue = 1, intPos = anyPosition}], tupleLitPos = anyPosition}] Nothing)
        it "Should parse single parenthesized expression (not tuple)" $
            parseProgram "(1)" parserCompilerFlags
                `shouldBe` Right
                    (Program [IntLit{intValue = 1, intPos = anyPosition}] Nothing)
        it "Should parse tuple type" $
            parseProgram "let x : (Int, String) = (1, \"test\")" parserCompilerFlags
                `shouldBe` Right
                    (Program [Function{def = [FuncDef{name = "x", args = [], body = TupleLit{tupleLitExprs = [IntLit{intValue = 1, intPos = anyPosition}, StringLit{stringValue = "test", stringPos = anyPosition}], tupleLitPos = anyPosition}, funcDefPos = anyPosition}], dec = FuncDec{name = "x", types = [Tuple [StructT "Int" [], StructT "String" []]], generics = [], funcDecPos = anyPosition}, functionPos = anyPosition}] Nothing)
        it "Should parse tuple access" $
            parseProgram "tuple.0" parserCompilerFlags
                `shouldBe` Right
                    (Program [TupleAccess{tupleAccessTuple = Var{varName = "tuple", varPos = anyPosition}, tupleAccessIndex = 0, tupleAccessPos = anyPosition}] Nothing)
        it "Should parse tuple access with index 1" $
            parseProgram "tuple.1" parserCompilerFlags
                `shouldBe` Right
                    (Program [TupleAccess{tupleAccessTuple = Var{varName = "tuple", varPos = anyPosition}, tupleAccessIndex = 1, tupleAccessPos = anyPosition}] Nothing)
        it "Should parse nested tuple types" $
            parseProgram "let x : ((Int, String), Bool) = ((1, \"test\"), True)" parserCompilerFlags
                `shouldBe` Right
                    (Program [Function{def = [FuncDef{name = "x", args = [], body = TupleLit{tupleLitExprs = [TupleLit{tupleLitExprs = [IntLit{intValue = 1, intPos = anyPosition}, StringLit{stringValue = "test", stringPos = anyPosition}], tupleLitPos = anyPosition}, BoolLit{boolValue = True, boolPos = anyPosition}], tupleLitPos = anyPosition}, funcDefPos = anyPosition}], dec = FuncDec{name = "x", types = [Tuple [Tuple [StructT "Int" [], StructT "String" []], StructT "Bool" []]], generics = [], funcDecPos = anyPosition}, functionPos = anyPosition}] Nothing)
        it "Should parse single-element parentheses as parenthesized expression, not tuple" $
            parseProgram "(x)" parserCompilerFlags
                `shouldBe` Right
                    (Program [Var{varName = "x", varPos = anyPosition}] Nothing)
        it "Should parse tuple pattern in function definition" $
            parseProgram "f (x, y) = x" parserCompilerFlags
                `shouldBe` Right
                    (Program [FuncDef{name = "f", args = [TupleLit{tupleLitExprs = [Var{varName = "x", varPos = anyPosition}, Var{varName = "y", varPos = anyPosition}], tupleLitPos = anyPosition}], body = Var{varName = "x", varPos = anyPosition}, funcDefPos = anyPosition}] Nothing)
        it "Should parse tuple pattern with literals" $
            parseProgram "f (1, 2) = 3" parserCompilerFlags
                `shouldBe` Right
                    (Program [FuncDef{name = "f", args = [TupleLit{tupleLitExprs = [IntLit{intValue = 1, intPos = anyPosition}, IntLit{intValue = 2, intPos = anyPosition}], tupleLitPos = anyPosition}], body = IntLit{intValue = 3, intPos = anyPosition}, funcDefPos = anyPosition}] Nothing)
    describe "Type parsing" $ do
        it "Should parse single parenthesized type as the type itself" $
            parseProgram "let x : (Int) = 1" parserCompilerFlags
                `shouldBe` Right
                    (Program [Function{def = [FuncDef{name = "x", args = [], body = IntLit{intValue = 1, intPos = anyPosition}, funcDefPos = anyPosition}], dec = FuncDec{name = "x", types = [StructT "Int" []], generics = [], funcDecPos = anyPosition}, functionPos = anyPosition}] Nothing)
        it "Should parse function type with one argument" $
            parseProgram "let x : (Int -> String) = \"test\"" parserCompilerFlags
                `shouldBe` Right
                    (Program [Function{def = [FuncDef{name = "x", args = [], body = StringLit{stringValue = "test", stringPos = anyPosition}, funcDefPos = anyPosition}], dec = FuncDec{name = "x", types = [AST.Fn [StructT "Int" []] (StructT "String" [])], generics = [], funcDecPos = anyPosition}, functionPos = anyPosition}] Nothing)
        it "Should parse thunk function type (zero arguments)" $
            parseProgram "let x : (-> String) = \"test\"" parserCompilerFlags
                `shouldBe` Right
                    (Program [Function{def = [FuncDef{name = "x", args = [], body = StringLit{stringValue = "test", stringPos = anyPosition}, funcDefPos = anyPosition}], dec = FuncDec{name = "x", types = [AST.Fn [] (StructT "String" [])], generics = [], funcDecPos = anyPosition}, functionPos = anyPosition}] Nothing)
        it "Should parse function type with multiple arguments" $
            parseProgram "let x : (Int -> String -> Bool) = True" parserCompilerFlags
                `shouldBe` Right
                    (Program [Function{def = [FuncDef{name = "x", args = [], body = BoolLit{boolValue = True, boolPos = anyPosition}, funcDefPos = anyPosition}], dec = FuncDec{name = "x", types = [AST.Fn [StructT "Int" [], StructT "String" []] (StructT "Bool" [])], generics = [], funcDecPos = anyPosition}, functionPos = anyPosition}] Nothing)
        it "Should parse nested parenthesized type" $
            parseProgram "let x : ((Int)) = 1" parserCompilerFlags
                `shouldBe` Right
                    (Program [Function{def = [FuncDef{name = "x", args = [], body = IntLit{intValue = 1, intPos = anyPosition}, funcDefPos = anyPosition}], dec = FuncDec{name = "x", types = [StructT "Int" []], generics = [], funcDecPos = anyPosition}, functionPos = anyPosition}] Nothing)
        it "Should parse nested function type" $
            parseProgram "let x : ((Int -> String)) = \"test\"" parserCompilerFlags
                `shouldBe` Right
                    (Program [Function{def = [FuncDef{name = "x", args = [], body = StringLit{stringValue = "test", stringPos = anyPosition}, funcDefPos = anyPosition}], dec = FuncDec{name = "x", types = [AST.Fn [StructT "Int" []] (StructT "String" [])], generics = [], funcDecPos = anyPosition}, functionPos = anyPosition}] Nothing)
        it "Should parse single-element tuple type with trailing comma" $
            parseProgram "let x : (Int,) = 1" parserCompilerFlags
                `shouldBe` Right
                    (Program [Function{def = [FuncDef{name = "x", args = [], body = IntLit{intValue = 1, intPos = anyPosition}, funcDefPos = anyPosition}], dec = FuncDec{name = "x", types = [Tuple [StructT "Int" []]], generics = [], funcDecPos = anyPosition}, functionPos = anyPosition}] Nothing)
