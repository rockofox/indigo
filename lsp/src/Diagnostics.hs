module Diagnostics where

import AST (Position (..), Program (..))
import BytecodeCompiler (CompilerError (..), CompilerState, compileProgram, errors, initCompilerState)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (evalStateT)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import ErrorRenderer (SourceError (..), parseErrorBundleToSourceErrors)
import IndigoPrelude (getPreludeExprs)
import Language.LSP.Protocol.Types
    ( Diagnostic (..)
    , DiagnosticSeverity (..)
    , Range
    )
import Parser (Program, initCompilerFlags, parseProgram)
import Position (indigoToLspRange)
import Text.Megaparsec (ParseErrorBundle)

sourceErrorToDiagnostic :: SourceError -> Text -> Maybe Diagnostic
sourceErrorToDiagnostic (SourceError{errorMessage = msg, errorPosition = pos}) sourceText = do
    range <- indigoToLspRange pos sourceText
    Just $
        Diagnostic
            { _range = range
            , _severity = Just DiagnosticSeverity_Error
            , _code = Nothing
            , _codeDescription = Nothing
            , _source = Just "indigo"
            , _message = T.pack msg
            , _tags = Nothing
            , _relatedInformation = Nothing
            , _data_ = Nothing
            }

compilerErrorToDiagnostic :: CompilerError -> Text -> Maybe Diagnostic
compilerErrorToDiagnostic (CompilerError{errorMessage = msg, errorPosition = pos, errorFile = _}) sourceText = do
    range <- indigoToLspRange pos sourceText
    Just $
        Diagnostic
            { _range = range
            , _severity = Just DiagnosticSeverity_Error
            , _code = Nothing
            , _codeDescription = Nothing
            , _source = Just "indigo"
            , _message = T.pack msg
            , _tags = Nothing
            , _relatedInformation = Nothing
            , _data_ = Nothing
            }

getDiagnostics :: Text -> IO [Diagnostic]
getDiagnostics sourceText = do
    preludeExprs <- getPreludeExprs
    let parseResult = parseProgram sourceText initCompilerFlags
    case parseResult of
        Left parseErrorBundle -> do
            let sourceErrors = parseErrorBundleToSourceErrors parseErrorBundle sourceText
            return $ mapMaybe (`sourceErrorToDiagnostic` sourceText) sourceErrors
        Right (Program userExprs) -> do
            let programWithPrelude = Program (preludeExprs ++ userExprs)
            compileResult <- evalStateT (compileProgram programWithPrelude) (initCompilerState programWithPrelude)
            case compileResult of
                Left _ -> return []
                Right compilerErrors -> do
                    return $ mapMaybe (`compilerErrorToDiagnostic` sourceText) compilerErrors
