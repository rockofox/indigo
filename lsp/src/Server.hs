{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import Completion (getCompletionsForPosition, resolveCompletionItem)
import Control.Exception (SomeException, catch)
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text qualified as T
import Diagnostics (getDiagnostics)
import Hover (getHover)
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Language.LSP.VFS (virtualFileText)

handlers :: Handlers (LspM ())
handlers =
    mconcat
        [ notificationHandler SMethod_Initialized $ \_ -> do
            return ()
        , notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_ -> do
            return ()
        , notificationHandler SMethod_SetTrace $ \_ -> do
            return ()
        , notificationHandler SMethod_WorkspaceDidChangeWatchedFiles $ \_ -> do
            return ()
        , notificationHandler SMethod_TextDocumentDidOpen $ \TNotificationMessage{_params = DidOpenTextDocumentParams doc} -> do
            let TextDocumentItem{_uri, _text} = doc
            updateDiagnostics _uri _text
        , notificationHandler SMethod_TextDocumentDidChange $ \TNotificationMessage{_params = DidChangeTextDocumentParams docId changes} -> do
            let VersionedTextDocumentIdentifier{_uri} = docId
            let normUri = toNormalizedUri (docId ^. uri)
            vf <- getVirtualFile normUri
            let sourceText = maybe T.empty virtualFileText vf
            updateDiagnostics _uri sourceText
        , notificationHandler SMethod_TextDocumentDidClose $ \_ -> do
            return ()
        , notificationHandler SMethod_CancelRequest $ \_ -> do
            return ()
        , requestHandler SMethod_TextDocumentHover $ \TRequestMessage{_params = HoverParams doc pos _workDone} responder -> do
            let TextDocumentIdentifier{_uri} = doc
            let normUri = toNormalizedUri _uri
            vf <- getVirtualFile normUri
            let sourceText = maybe T.empty virtualFileText vf
            hoverResult <- getHover sourceText pos
            responder $ Right $ maybe (InR Null) InL hoverResult
        , requestHandler SMethod_TextDocumentCompletion $ \TRequestMessage{_params = CompletionParams doc pos _context _workDone _partialResult} responder -> do
            let TextDocumentIdentifier{_uri} = doc
            let normUri = toNormalizedUri _uri
            vf <- getVirtualFile normUri
            let sourceText = maybe T.empty virtualFileText vf
            completionResult <- getCompletionsForPosition sourceText pos
            let completions = case completionResult of
                    Right cl -> cl
                    Left _ -> CompletionList False Nothing []
            responder $ Right $ InR $ InL completions
        , requestHandler SMethod_TextDocumentDiagnostic $ \TRequestMessage{_params = DocumentDiagnosticParams{_textDocument = TextDocumentIdentifier{_uri}}} responder -> do
            let normUri = toNormalizedUri _uri
            vf <- getVirtualFile normUri
            let text = maybe T.empty virtualFileText vf
            diagnostics <- liftIO $ catch (getDiagnostics text) $ \(_ :: SomeException) -> do
                return []
            let fullDiagReport =
                    RelatedFullDocumentDiagnosticReport
                        { _kind = AString
                        , _resultId = Nothing
                        , _items = diagnostics
                        , _relatedDocuments = Nothing
                        }
            responder $ Right $ DocumentDiagnosticReport $ InL fullDiagReport
        , requestHandler SMethod_CompletionItemResolve $ \TRequestMessage{_params = item} responder -> do
            let resolvedItem = resolveCompletionItem item
            responder $ Right resolvedItem
        ]

updateDiagnostics :: Uri -> Text -> LspM () ()
updateDiagnostics uri text = do
    diagnostics <- liftIO $ catch (getDiagnostics text) $ \(_ :: SomeException) -> do
        return []
    let params = PublishDiagnosticsParams uri Nothing diagnostics
    sendNotification SMethod_TextDocumentPublishDiagnostics params
