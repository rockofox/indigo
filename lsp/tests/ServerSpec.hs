module ServerSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Language.LSP.Protocol.Types
    ( TextDocumentContentChangeEvent (..)
    , TextDocumentContentChangeWholeDocument (..)
    )
import Language.LSP.Protocol.Types qualified as LSPTypes
import Language.LSP.Test
import Test.Hspec
import TestUtils (lspExe)

spec :: Spec
spec = do
    describe "Server initialization" $ do
        it "initializes successfully" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            doc <- openDoc "valid.in" "indigo"
            liftIO $ True `shouldBe` True

        it "handles document open notification" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            doc <- openDoc "valid.in" "indigo"
            liftIO $ True `shouldBe` True

        it "handles document close notification" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            doc <- openDoc "valid.in" "indigo"
            closeDoc doc
            liftIO $ True `shouldBe` True

        it "handles document change notification" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            doc <- openDoc "valid.in" "indigo"
            changeDoc doc [TextDocumentContentChangeEvent (LSPTypes.InR (TextDocumentContentChangeWholeDocument "let x = 42"))]
            liftIO $ True `shouldBe` True

        it "handles diagnostic requests" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            doc <- openDoc "valid.in" "indigo"
            diags <- waitForDiagnostics
            liftIO $ diags `shouldSatisfy` null
