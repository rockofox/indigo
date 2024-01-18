module VerifierSpec (spec) where

import AST (Expr (..), Type (..), exprs, zeroPosition)
import Control.Applicative qualified as Set
import Data.List.NonEmpty ()
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty qualified as Set
import Data.Text qualified as T
import GHC.IO (unsafePerformIO)
import Parser (parseProgramPure)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Megaparsec (errFancy, fancy, shouldFailWith)
import Text.Megaparsec (ErrorFancy (ErrorFail))
import Text.Megaparsec.Error.Builder ()
import Text.RawString.QQ (r)
import Verifier (verifyProgram)

spec :: Spec
spec = do
    describe "verifyProgram" $ do
        it "Should fail on undefined variables" $ do
            unsafePerformIO (verifyProgram "test.in" [r|f x y = x + z|] [FuncDec "f" [StructT "Int", StructT "Int", StructT "Int"] [], FuncDef "f" [Var "x" zeroPosition, Var "y" zeroPosition] (Add (Var "x" zeroPosition) (Var "z" zeroPosition))])
                `shouldFailWith` errFancy 0 (fancy $ ErrorFail "Could not find relevant binding for z")
        it "Should fail on undefined functions" $ do
            unsafePerformIO (verifyProgram "test.in" [r|let main => IO = x 2|] [FuncDec "main" [StructT "IO"] [], FuncDef "main" [] (FuncCall "x" [IntLit 2] zeroPosition)])
                `shouldFailWith` errFancy 0 (fancy $ ErrorFail "Could not find relevant binding for x")
    describe "Generics" $ do
        it "Should fail on non match" $ do
            let prog =
                    [r|
                    trait Animal

                    struct Dog = (name: String)
                    struct Cat = (name: String)

                    impl Animal for Dog
                    impl Animal for Cat

                    let like<T: Animal> (a: T b: T) => IO = do
                        println a.name : " likes " : b.name : "!"
                    end

                    let main => IO = do
                        like (Dog { name : "Bello" }), (Cat { name : "Mauzi" })
                    end
                |]
            unsafePerformIO (verifyProgram "test.in" prog (AST.exprs $ parseProgramPure prog))
                `shouldFailWith` errFancy 431 (fancy $ ErrorFail "Argument types do not match on like, expected: [T,T], got: [Dog,Cat]")
        it "Should succeed on match" $ do
            let prog =
                    [r|
                    trait Animal

                    struct Dog = (name: String)
                    struct Cat = (name: String)

                    impl Animal for Dog
                    impl Animal for Cat

                    let like<T: Animal> (a: T b: T) => IO = do
                        println a.name : " likes " : b.name : "!"
                    end

                    let main => IO = do
                        like Cat { name : "Mauzi" }, Cat { name : "Murri" }
                    end
                |]
            unsafePerformIO (verifyProgram "test.in" prog (AST.exprs $ parseProgramPure prog))
                `shouldBe` Right ()
