module DiagnosticsSpec (spec) where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as T
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Types
    ( DiagnosticSeverity (..)
    , TextDocumentContentChangeEvent (..)
    , TextDocumentContentChangeWholeDocument (..)
    )
import Language.LSP.Protocol.Types qualified as LSPTypes
import Language.LSP.Test
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)
import TestUtils (lspExe)

spec :: Spec
spec = do
    describe "Diagnostics" $ do
        it "reports parse errors" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            _doc <- openDoc "parse_error.in" "indigo"
            diags <- waitForDiagnostics
            liftIO $ do
                Prelude.length diags `shouldSatisfy` (> 0)
                case diags of
                    diag : _ -> do
                        diag ^. severity `shouldBe` Just DiagnosticSeverity_Error
                        diag ^. source `shouldBe` Just "indigo"
                        T.length (diag ^. Language.LSP.Protocol.Lens.message) `shouldSatisfy` (> 0)
                    [] -> Prelude.error "Expected diagnostics"

        it "reports compilation errors" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            _doc <- openDoc "compile_error.in" "indigo"
            diags <- waitForDiagnostics
            liftIO $ do
                Prelude.length diags `shouldSatisfy` (> 0)
                case diags of
                    diag : _ -> do
                        diag ^. severity `shouldBe` Just DiagnosticSeverity_Error
                        diag ^. source `shouldBe` Just "indigo"
                    [] -> Prelude.error "Expected diagnostics"

        it "reports no errors for valid code" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            _doc <- openDoc "valid.in" "indigo"
            diags <- waitForDiagnostics
            liftIO $ do
                Prelude.length diags `shouldBe` 0

        it "updates diagnostics on document change" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            doc <- openDoc "parse_error.in" "indigo"
            diags1 <- waitForDiagnostics
            changeDoc doc [TextDocumentContentChangeEvent (LSPTypes.InR (TextDocumentContentChangeWholeDocument "let x = 42"))]
            diags2 <- waitForDiagnostics
            liftIO $ do
                Prelude.length diags1 `shouldSatisfy` (> 0)
                Prelude.length diags2 `shouldSatisfy` (<= Prelude.length diags1)

        it "reports error for missing module" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            _doc <- openDoc "diagnostics_missing_module.in" "indigo"
            diags <- waitForDiagnostics
            liftIO $ do
                Prelude.length diags `shouldSatisfy` (> 0)
                case diags of
                    diag : _ -> do
                        diag ^. severity `shouldBe` Just DiagnosticSeverity_Error
                        diag ^. source `shouldBe` Just "indigo"
                        let msg = diag ^. Language.LSP.Protocol.Lens.message
                        T.pack "not found" `T.isInfixOf` msg `shouldBe` True
                    [] -> expectationFailure "Expected diagnostic for missing module"
