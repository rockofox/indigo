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
            parseProgram "let main => IO = print \"Hello, world!\"" parserCompilerFlags
                `shouldBe` Right
                    (Program [Function{def = [FuncDef{name = "main", args = [], body = FuncCall "print" [StringLit "Hello, world!"] anyPosition}], dec = FuncDec{name = "main", types = [StructT "IO"], generics = []}}])
    describe "Struct" $ do
        it "Member access" $ do
            parseProgram "bello{}.name" parserCompilerFlags
                `shouldBe` Right
                    (Program [StructAccess (StructLit "bello" [] anyPosition) (Var "name" anyPosition)])
    describe "Traits" $ do
        it "Should parse a trait decleration" $
            parseProgram "trait Show = do\nshow :: Self -> String\nend" parserCompilerFlags
                `shouldBe` Right
                    (Program [Trait{name = "Show", methods = [FuncDec{name = "show", types = [Self, StructT "String"], generics = []}]}])
        it "Should parse a trait declaration with multiple methods" $
            parseProgram "trait Show = do\nshow :: Self -> String\nshow2 :: Self -> String\nend" parserCompilerFlags
                `shouldBe` Right
                    (Program [Trait{name = "Show", methods = [FuncDec{name = "show", types = [Self, StructT "String"], generics = []}, FuncDec{name = "show2", types = [Self, StructT "String"], generics = []}]}])
        it "Should parse a trait implementation" $
            parseProgram "impl Show for Point = do\nshow point = \"Point {x: \" : show point.x : \", y: \" : show point.y : \"}\"\nend" parserCompilerFlags
                `shouldBe` Right
                    ( Program
                        [ Impl
                            { trait = "Show"
                            , for = "Point"
                            , methods =
                                [ FuncDef{name = "show", args = [Var "point" anyPosition], body = parseFreeUnsafe "\"Point {x: \" : show point.x : \", y: \" : show point.y : \"}\""}
                                ]
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
                    (Program [FuncDec{name = "+", types = [Any, Any, Any], generics = []}, FuncDec{name = "-", types = [Any, Any, Any], generics = []}, FuncDec{name = "*", types = [Any, Any, Any], generics = []}, FuncDec{name = "/", types = [Any, Any, Any], generics = []}, FuncDec{name = "==", types = [Any, Any, Any], generics = []}])
        it "Should be able to use gravis escaped functions in calls" $
            do
                parseProgram
                    [r|
                filter (`==`1), [1, 2, 3]
            |]
                    parserCompilerFlags
                `shouldBe` Right
                    (Program [FuncCall "filter" [FuncCall "==" [IntLit 1] anyPosition, ListLit [IntLit 1, IntLit 2, IntLit 3]] anyPosition])
    describe "Unary minus" $ do
        it "Should parse unary minus" $
            parseProgram "-1" parserCompilerFlags
                `shouldBe` Right
                    (Program [UnaryMinus (IntLit 1)])
        it "Should be able to use negative numbers in multiplication" $
            parseProgram "1 *-1" parserCompilerFlags
                `shouldBe` Right
                    (Program [Mul (IntLit 1) (UnaryMinus (IntLit 1))])
        it "Should be able to negate expressions in parentheses" $
            parseProgram "2 * -(3-x*5)" parserCompilerFlags
                `shouldBe` Right
                    (Program [Mul (IntLit 2) (UnaryMinus (Sub (IntLit 3) (Mul (Var "x" anyPosition) (IntLit 5))))])
