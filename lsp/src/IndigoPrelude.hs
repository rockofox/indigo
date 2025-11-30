{-# LANGUAGE ScopedTypeVariables #-}

module IndigoPrelude where

import AST (Expr, Program (..))
import BytecodeCompiler (preludeFile)
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text qualified as T
import Parser (initCompilerFlags, needsMain, parseProgram)

loadPrelude :: (MonadIO m) => m (Either String [Expr])
loadPrelude = do
    result <- liftIO $ try preludeFile
    case result of
        Left (e :: SomeException) -> return $ Left $ "Failed to load prelude: " ++ show e
        Right preludeContent -> do
            let parseResult = parseProgram (T.pack preludeContent) initCompilerFlags{needsMain = False}
            case parseResult of
                Left _ -> return $ Left "Failed to parse prelude"
                Right (Program exprs) -> return $ Right exprs

getPreludeExprs :: (MonadIO m) => m [Expr]
getPreludeExprs = do
    result <- loadPrelude
    case result of
        Left _ -> return []
        Right exprs -> return exprs
