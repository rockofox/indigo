module VerifierSpec (spec) where

import AST (Expr (..), Type (..), zeroPosition)
import Control.Applicative qualified as Set
import Data.List.NonEmpty ()
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty qualified as Set
import Data.Text qualified as T
import GHC.IO (unsafePerformIO)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec (errFancy, fancy, shouldFailWith)
import Text.Megaparsec (ErrorFancy (ErrorFail))
import Text.Megaparsec.Error.Builder ()
import Text.RawString.QQ (r)
import Verifier (verifyProgram)

spec :: Spec
spec = do
    describe "verifyProgram" $ do
        it "Should fail on undefined variables" $ do
            unsafePerformIO (verifyProgram "test.in" [r|f x y = x + z|] [FuncDec "f" [Int, Int, Int], FuncDef "f" [Var "x" zeroPosition, Var "y" zeroPosition] (Add (Var "x" zeroPosition) (Var "z" zeroPosition))])
                `shouldFailWith` errFancy 0 (fancy $ ErrorFail "Could not find relevant binding for z")
        it "Should fail on undefined functions" $ do
            unsafePerformIO (verifyProgram "test.in" [r|let main => IO = x 2|] [FuncDec "main" [IO], FuncDef "main" [] (FuncCall "x" [IntLit 2] zeroPosition)])
                `shouldFailWith` errFancy 0 (fancy $ ErrorFail "Could not find relevant binding for x")
