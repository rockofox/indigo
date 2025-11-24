module VerifierSpec (spec) where

import AST (Expr (..), Type (..), exprs, zeroPosition)
import Control.Applicative qualified as Set
import Control.Monad (unless)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty qualified as Set
import Data.Text qualified as T
import GHC.IO (unsafePerformIO)
import Parser (parseProgramPure)
import Test.Hspec (Expectation, HasCallStack, Spec, describe, expectationFailure, it, shouldBe)
import Test.Hspec.Megaparsec (errFancy, fancy, shouldFailWith)
import Text.Megaparsec
import Text.Megaparsec.Error (ErrorFancy (ErrorFail))
import Text.Megaparsec.Error.Builder
import Text.RawString.QQ (r)
import Verifier (verifyProgram)

spec :: Spec
spec = do
    -- no op
    return ()

-- describe "verifyProgram" $ do
--     it "Should fail on undefined variables" $ do
--         unsafePerformIO (verifyProgram "test.in" [r|f x y = x + z|] [FuncDec "f" [StructT "Int", StructT "Int", StructT "Int"] [], FuncDef "f" [Var "x" zeroPosition, Var "y" zeroPosition] (Add (Var "x" zeroPosition) (Var "z" zeroPosition))])
--             `shouldFailWith` errFancy 0 (fancy $ ErrorFail "Could not find relevant binding for z")
--     it "Should fail on undefined functions" $ do
--         unsafePerformIO (verifyProgram "test.in" [r|let main : IO = x 2|] [FuncDec "main" [StructT "IO"] [], FuncDef "main" [] (FuncCall "x" [IntLit 2] zeroPosition)])
--             `shouldFailWith` errFancy 0 (fancy $ ErrorFail "Could not find relevant binding for x")
-- describe "Generics" $ do
--     it "Should fail on non match" $ do
--         let prog =
--                 [r|
--                 trait Animal
--
--                 struct Dog = (name: String)
--                 struct Cat = (name: String)
--
--                 impl Animal for Dog
--                 impl Animal for Cat
--
--                 let like<T: Animal> (a: T b: T) : IO = do
--                     println a.name : " likes " : b.name : "!"
--                 end
--
--                 let main : IO = do
--                     like (Dog { name : "Bello" }), (Cat { name : "Mauzi" })
--                 end
--             |]
--         unsafePerformIO (verifyProgram "test.in" prog (AST.exprs $ parseProgramPure prog))
--             `shouldFailWith` errFancy 429 (fancy $ ErrorFail "Argument types do not match on like, expected: [T,T], got: [Dog,Cat]")
--     it "Should succeed on match" $ do
--         let prog =
--                 [r|
--                 trait Animal
--
--                 struct Dog = (name: String)
--                 struct Cat = (name: String)
--
--                 impl Animal for Dog
--                 impl Animal for Cat
--
--                 let like<T: Animal> (a: T b: T) : IO = do
--                     println a.name : " likes " : b.name : "!"
--                 end
--
--                 let main : IO = do
--                     like Cat { name : "Mauzi" }, Cat { name : "Murri" }
--                 end
--             |]
--         unsafePerformIO (verifyProgram "test.in" prog (AST.exprs $ parseProgramPure prog))
--             `shouldBe` Right ()
--     it "Should success on a list example" $ do
--         let prog =
--                 [r|
--                 trait Number
--                 impl Number for Int
--                 impl Number for Float
--
--                 let listTest<N: Number> (a: [N] b: N) : N = do
--                 end
--
--                 let main : IO = do
--                     println listTest [1, 2, 3], 4
--                 end
--             |]
--         unsafePerformIO (verifyProgram "test.in" prog (AST.exprs $ parseProgramPure prog))
--             `shouldBe` Right ()
--     it "Can fail on list example" $ do
--         let prog =
--                 [r|
--                 trait Number
--                 impl Number for Int
--                 impl Number for Float
--
--                 let listTest<N: Number> (a: [N] b: N) : N = do
--                 end
--
--                 let main : IO = do
--                     println listTest [1, 2, 3], "4"
--                 end
--             |]
--         unsafePerformIO (verifyProgram "test.in" prog (AST.exprs $ parseProgramPure prog))
--             `shouldFailWith` errFancy 280 (fancy $ ErrorFail "Argument types do not match on listTest, expected: [[N],N], got: [[Int],String]")
--     it "Can type erase on return type" $ do
--         let prog =
--                 [r|
--             trait Number
--             impl Number for Int
--             impl Number for Float
--
--             let add<N: Number> (a: N b: N) : N = do
--                 a + b
--             end
--
--             let xxx (a: Int) : Int = do
--                 a
--             end
--
--             let main : IO = do
--                 println xxx (add 1, 2)
--             end
--         |]
--         unsafePerformIO (verifyProgram "test.in" prog (AST.exprs $ parseProgramPure prog))
--             `shouldFailWith` errFancy 374 (fancy $ ErrorFail "Argument types do not match on xxx, expected: [Int], got: [Number]")
-- describe "Refinement types" $ do
--     it "Can fail on refinement" $ do
--         let prog =
--                 [r|
--                     struct AlcoholConsumer = (age: Int) satisfies (it.age > 18)
--                     let main : IO = do
--                         let a = AlcoholConsumer { age : 19 }
--                         let b = AlcoholConsumer { age : 17 }
--                     end
--                 |]
--         unsafePerformIO (verifyProgram "test.in" prog (AST.exprs $ parseProgramPure prog))
--             `shouldFailWith` errFancy 229 (fancy $ ErrorFail "Refinement failed (it.age > 18)")
