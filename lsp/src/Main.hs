module Main where

import Control.Lens (Iso (..))
import Control.Monad.IO.Class (liftIO)
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Server (handlers)
import System.IO

main :: IO Int
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    runServer serverDefinition

serverDefinition :: ServerDefinition ()
serverDefinition =
    ServerDefinition
        { parseConfig = const $ const $ Right ()
        , onConfigChange = const $ pure ()
        , defaultConfig = ()
        , configSection = "indigo"
        , doInitialize = \env _req -> do
            -- The library's inferServerCapabilities replaces capabilities from doInitialize
            -- So we just return env and set capabilities via Options
            pure $ Right env
        , staticHandlers = const handlers
        , interpretHandler = \env -> Iso (runLspT env) liftIO
        , options =
            defaultOptions
                { optTextDocumentSync =
                    Just $
                        TextDocumentSyncOptions
                            { _openClose = Just True
                            , _change = Just TextDocumentSyncKind_Incremental
                            , _willSave = Nothing
                            , _willSaveWaitUntil = Nothing
                            , _save = Nothing
                            }
                , optCompletionTriggerCharacters = Just ['.']
                }
        }
