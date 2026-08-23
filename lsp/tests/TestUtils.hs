module TestUtils (lspExe) where

import System.Directory (findExecutable, getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE lspExe #-}
lspExe :: String
lspExe = unsafePerformIO $ do
    mEnv <- lookupEnv "INDIGO_LSP_EXE"
    case mEnv of
        Just path -> pure path
        Nothing -> do
            mPath <- findExecutable "indigo-lsp"
            case mPath of
                Just path -> pure path
                Nothing -> do
                    home <- getHomeDirectory
                    pure $ home </> ".cabal" </> "bin" </> "indigo-lsp"
