module TestUtils (lspExe) where

import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE lspExe #-}
lspExe :: String
lspExe = unsafePerformIO $ do
    home <- getHomeDirectory
    return $ home </> ".cabal" </> "bin" </> "indigo-lsp"
