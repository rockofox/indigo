module ParserSpec (spec) where

import AST
import GHC.Generics
import Parser
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Text.RawString.QQ (r)

instance Arbitrary Type where
    arbitrary = genericArbitrary
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
                    (Program [FuncCall{funcName = ":=", funcArgs = [Var{varName = "x", varPos = Position (0, 2)}, IntLit{intValue = 2, intPos = anyPosition}], funcPos = anyPosition}])
    describe "Function definitions" $ do
        it "Should parse a simple function" $
            parseProgram "add a b = a + b" parserCompilerFlags
                `shouldBe` Right
                    (Program [FuncDef{name = "add", args = [Var{varName = "a", varPos = anyPosition}, Var{varName = "b", varPos = anyPosition}], body = Add{addLhs = Var{varName = "a", varPos = anyPosition}, addRhs = Var{varName = "b", varPos = anyPosition}, addPos = anyPosition}, funcDefPos = anyPosition}])
        it "Should prase a function with 0 parameters" $
            parseProgram "thunk = 1" parserCompilerFlags
                `shouldBe` Right
                    (Program [FuncDef{name = "thunk", args = [], body = IntLit{intValue = 1, intPos = anyPosition}, funcDefPos = anyPosition}])
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
                \t -> compareTypes (StructT t) (StructT t) `shouldBe` True
    describe "Basic" $ do
        it "Should parse a simple program" $
            parseProgram "let main : IO = print \"Hello, world!\"" parserCompilerFlags
                `shouldBe` Right
                    (Program [Function{def = [FuncDef{name = "main", args = [], body = FuncCall{funcName = "print", funcArgs = [StringLit{stringValue = "Hello, world!", stringPos = anyPosition}], funcPos = anyPosition}, funcDefPos = anyPosition}], dec = FuncDec{name = "main", types = [StructT "IO"], generics = [], funcDecPos = anyPosition}, functionPos = anyPosition}])
    describe "Struct" $ do
        it "Member access" $ do
            parseProgram "bello{}.name" parserCompilerFlags
                `shouldBe` Right
                    (Program [StructAccess{structAccessStruct = StructLit{structLitName = "bello", structLitFields = [], structLitPos = anyPosition}, structAccessField = Var{varName = "name", varPos = anyPosition}, structAccessPos = anyPosition}])
        it "Can define" $
            parseProgram "struct Teacher = ()" parserCompilerFlags
                `shouldBe` Right (Program [Struct{name = "Teacher", fields = [], refinement = Nothing, refinementSrc = "", is = [], structPos = anyPosition}])
        it "Can define with is" $ do
            parseProgram "struct Teacher = () is Person" parserCompilerFlags
                `shouldBe` Right (Program [Struct{name = "Teacher", fields = [], refinement = Nothing, refinementSrc = "", is = ["Person"], structPos = anyPosition}])
        it "Can define with multiple is" $ do
            parseProgram "struct Teacher = () is Person, Employee" parserCompilerFlags
                `shouldBe` Right (Program [Struct{name = "Teacher", fields = [], refinement = Nothing, refinementSrc = "", is = ["Person", "Employee"], structPos = anyPosition}])
    describe "Traits" $ do
        it "Should parse a trait decleration" $
            parseProgram "trait Show = do\nshow :: Self -> String\nend" parserCompilerFlags
                `shouldBe` Right
                    (Program [Trait{name = "Show", methods = [FuncDec{name = "show", types = [Self, StructT "String"], generics = [], funcDecPos = anyPosition}], traitPos = anyPosition}])
        it "Should parse a trait declaration with multiple methods" $
            parseProgram "trait Show = do\nshow :: Self -> String\nshow2 :: Self -> String\nend" parserCompilerFlags
                `shouldBe` Right
                    (Program [Trait{name = "Show", methods = [FuncDec{name = "show", types = [Self, StructT "String"], generics = [], funcDecPos = anyPosition}, FuncDec{name = "show2", types = [Self, StructT "String"], generics = [], funcDecPos = anyPosition}], traitPos = anyPosition}])
        it "Should parse a trait implementation" $
            parseProgram "impl Show for Point = do\nshow point = \"Point {x: \" : show point.x : \", y: \" : show point.y : \"}\"\nend" parserCompilerFlags
                `shouldBe` Right
                    ( Program
                        [ Impl
                            { trait = "Show"
                            , for = "Point"
                            , methods =
                                [ FuncDef{name = "show", args = [Var{varName = "point", varPos = anyPosition}], body = parseFreeUnsafe "\"Point {x: \" : show point.x : \", y: \" : show point.y : \"}\"", funcDefPos = anyPosition}
                                ]
                            , implPos = anyPosition
                            }
                        ]
                    )
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
                    (Program [FuncDec{name = "+", types = [Any, Any, Any], generics = [], funcDecPos = anyPosition}, FuncDec{name = "-", types = [Any, Any, Any], generics = [], funcDecPos = anyPosition}, FuncDec{name = "*", types = [Any, Any, Any], generics = [], funcDecPos = anyPosition}, FuncDec{name = "/", types = [Any, Any, Any], generics = [], funcDecPos = anyPosition}, FuncDec{name = "==", types = [Any, Any, Any], generics = [], funcDecPos = anyPosition}])
        it "Should be able to use gravis escaped functions in calls" $
            do
                parseProgram
                    [r|
                filter (`==`1), [1, 2, 3]
            |]
                    parserCompilerFlags
                `shouldBe` Right
                    (Program [FuncCall{funcName = "filter", funcArgs = [FuncCall{funcName = "==", funcArgs = [IntLit{intValue = 1, intPos = anyPosition}], funcPos = anyPosition}, ListLit{listLitExprs = [IntLit{intValue = 1, intPos = anyPosition}, IntLit{intValue = 2, intPos = anyPosition}, IntLit{intValue = 3, intPos = anyPosition}], listLitPos = anyPosition}], funcPos = anyPosition}])
    describe "Unary minus" $ do
        it "Should parse unary minus" $
            parseProgram "(-1)" parserCompilerFlags
                `shouldBe` Right
                    (Program [UnaryMinus{unaryMinusExpr = IntLit{intValue = 1, intPos = anyPosition}, unaryMinusPos = anyPosition}])
        it "Should be able to use negative numbers in multiplication" $
            parseProgram "1 * (-1)" parserCompilerFlags
                `shouldBe` Right
                    (Program [Mul{mulLhs = IntLit{intValue = 1, intPos = anyPosition}, mulRhs = UnaryMinus{unaryMinusExpr = IntLit{intValue = 1, intPos = anyPosition}, unaryMinusPos = anyPosition}, mulPos = anyPosition}])
        it "Should be able to negate expressions in parentheses" $
            parseProgram "2 * (-(3-x*5))" parserCompilerFlags
                `shouldBe` Right
                    (Program [Mul{mulLhs = IntLit{intValue = 2, intPos = anyPosition}, mulRhs = UnaryMinus{unaryMinusExpr = Sub{subLhs = IntLit{intValue = 3, intPos = anyPosition}, subRhs = Mul{mulLhs = Var{varName = "x", varPos = anyPosition}, mulRhs = IntLit{intValue = 5, intPos = anyPosition}, mulPos = anyPosition}, subPos = anyPosition}, unaryMinusPos = anyPosition}, mulPos = anyPosition}])
    describe "Parentheses" $ do
        it "Should parse parentheses" $
            parseProgram "(1 + 2) * 3" parserCompilerFlags
                `shouldBe` Right
                    (Program [Mul{mulLhs = Add{addLhs = IntLit{intValue = 1, intPos = anyPosition}, addRhs = IntLit{intValue = 2, intPos = anyPosition}, addPos = anyPosition}, mulRhs = IntLit{intValue = 3, intPos = anyPosition}, mulPos = anyPosition}])
        it "Should parse parenthesis application correctly" $
            parseProgram "(x y) z" parserCompilerFlags
                `shouldBe` Right
                    (Program [ParenApply{parenApplyExpr = FuncCall{funcName = "x", funcArgs = [Var{varName = "y", varPos = anyPosition}], funcPos = anyPosition}, parenApplyArgs = [Var{varName = "z", varPos = anyPosition}], parenApplyPos = anyPosition}])
        it "bottles (i)-1" $
            parseProgram "bottles (i)-1" parserCompilerFlags
                `shouldBe` Right
                    (Program [FuncCall{funcName = "bottles", funcArgs = [Sub{subLhs = Var{varName = "i", varPos = anyPosition}, subRhs = IntLit{intValue = 1, intPos = anyPosition}, subPos = anyPosition}], funcPos = anyPosition}])

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
                    (Program [Function{def = [FuncDef{name = "add", args = [Var{varName = "a", varPos = anyPosition}, Var{varName = "b", varPos = anyPosition}], body = DoBlock{doBlockExprs = [Add{addLhs = Var{varName = "a", varPos = anyPosition}, addRhs = Var{varName = "b", varPos = anyPosition}, addPos = anyPosition}], doBlockPos = anyPosition}, funcDefPos = anyPosition}], dec = FuncDec{name = "add", types = [StructT "N", StructT "N", StructT "N"], generics = [GenericExpr "N" (Just $ StructT "Number")], funcDecPos = anyPosition}, functionPos = anyPosition}])
        it "Should parse classic decleration generics" $ do
            parseProgram
                [r|
                add<N: Number> :: N -> N -> N
            |]
                parserCompilerFlags
                `shouldBe` Right
                    (Program [FuncDec{name = "add", types = [StructT "N", StructT "N", StructT "N"], generics = [GenericExpr "N" (Just $ StructT "Number")], funcDecPos = anyPosition}])
