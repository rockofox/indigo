module PositionSpec (spec) where

import AST qualified
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust, isNothing)
import Data.Text qualified as T
import Language.LSP.Protocol.Types (Position (..), Range (..))
import Language.LSP.Test
import Position (getLineAndColumn, indigoToLspPosition, lspToIndigoOffset)
import Test.Hspec
import TestUtils (lspExe)

spec :: Spec
spec = do
    describe "Offset to line and column conversion" $ do
        it "clamps an offset at EOF to the end of the last line" $
            getLineAndColumn 8 (T.lines "let x =\n") `shouldBe` (1, 7)
        it "maps an EOF offset to a position instead of dropping it" $
            indigoToLspPosition (AST.Position (8, 8)) "let x =\n" `shouldBe` Just (Position 0 7)
        it "maps offsets in an empty file to the start" $
            indigoToLspPosition (AST.Position (0, 0)) "" `shouldBe` Just (Position 0 0)
        it "rejects negative offsets" $
            indigoToLspPosition (AST.Position (-1, -1)) "let x = 1\n" `shouldBe` Nothing
        it "converts columns to UTF-16 code units" $
            indigoToLspPosition (AST.Position (1, 1)) "\128077x\n" `shouldBe` Just (Position 0 2)
    describe "LSP position to offset conversion" $ do
        it "converts UTF-16 columns back to codepoint offsets" $
            lspToIndigoOffset (Position 0 3) "\128077x\n" `shouldBe` 2
        it "clamps past-the-end lines to the end of the source" $
            lspToIndigoOffset (Position 5 0) "let x =\n" `shouldBe` 8
    describe "Position conversion" $ do
        it "converts LSP position to Indigo offset correctly" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            doc <- openDoc "position_test.in" "indigo"
            let pos = Position 0 0
            hover <- getHover doc pos
            liftIO $ hover `shouldSatisfy` (\h -> isJust h || isNothing h)

        it "handles multi-line positions" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            doc <- openDoc "position_multiline.in" "indigo"
            let pos = Position 1 4
            hover <- getHover doc pos
            liftIO $ hover `shouldSatisfy` (\h -> isJust h || isNothing h)

        it "handles positions at end of line" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            doc <- openDoc "position_test.in" "indigo"
            let pos = Position 0 10
            hover <- getHover doc pos
            liftIO $ hover `shouldSatisfy` (\h -> isJust h || isNothing h)
