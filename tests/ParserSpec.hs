module ParserSpec (spec) where

import AST
import GHC.Generics
import Parser
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic

instance Arbitrary Type where
    arbitrary = genericArbitrary
    shrink = genericShrink

isFn :: Type -> Bool
isFn (AST.Fn _ _) = True
isFn _ = False

spec :: Spec
spec = do
    describe "compareTypes" $ do
        it "Should be true for exact matches" $
            property $
                \t -> compareTypes t t `shouldBe` True
        xit "Should be false for non-exact matches (except for Any or Fn or Self)" $
            property $ -- Janky
                \t1 t2 ->
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
            parseProgram "let main => IO = print \"Hello, world!\"" initCompilerFlags
                `shouldBe` Right
                    (Program [Function{def = [FuncDef{name = "main", args = [], body = FuncCall "print" [StringLit "Hello, world!"] anyPosition}], dec = FuncDec{name = "main", types = [StructT "IO"]}}])
    describe "Struct" $ do
        it "Member access" $ do
            parseProgram "bello{}.name" initCompilerFlags
                `shouldBe` Right
                    (Program [StructAccess (StructLit "bello" []) (Var "name" anyPosition)])
    describe "Traits" $ do
        it "Should parse a trait decleration" $
            parseProgram "trait Show = do\nshow :: Self -> String\nend" initCompilerFlags
                `shouldBe` Right
                    (Program [Trait{name = "Show", methods = [FuncDec{name = "show", types = [Self, String]}]}])
        it "Should parse a trait declaration with multiple methods" $
            parseProgram "trait Show = do\nshow :: Self -> String\nshow2 :: Self -> String\nend" initCompilerFlags
                `shouldBe` Right
                    (Program [Trait{name = "Show", methods = [FuncDec{name = "show", types = [Self, String]}, FuncDec{name = "show2", types = [Self, String]}]}])
        it "Should parse a trait implementation" $
            parseProgram "impl Show for Point = do\nshow point = \"Point {x: \" : show point.x : \", y: \" : show point.y : \"}\"\nend" initCompilerFlags
                `shouldBe` Right
                    -- (Program [Impl{trait = "Show", for = "Point", methods = [parseFreeUnsafe "show point = \"Point {x: \" : show point.x : \", y: \" : show point.y : \"}\""]}])
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
