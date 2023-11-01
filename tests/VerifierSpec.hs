module VerifierSpec (spec) where

import AST (Expr (..), PosExpr (PosExpr))
import Control.Applicative qualified as Set
import Data.List.NonEmpty ()
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty qualified as Set
import Data.Text qualified as T
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
            verifyProgram "test.in" [r|f x y = x + z|] [PosExpr (FuncDef "f" [Var "x", Var "y"] (Add (Var "x") (Var "z")), 0, 0), PosExpr (Add (Var "x") (Var "z"), 0, 0)] `shouldFailWith` errFancy 0 (fancy $ ErrorFail "Variable z not defined in this scope")
        it "Should fail on undefined functions" $ do
            verifyProgram "test.in" [r|main => IO = x 2|] [PosExpr (FuncDef "main" [] (FuncCall "x" [IntLit 2]), 0, 0), PosExpr (FuncCall "x" [IntLit 2], 0, 0)] `shouldFailWith` errFancy 0 (fancy $ ErrorFail "Function x not defined")
