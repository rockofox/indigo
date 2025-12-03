module Diagnostics where

import AST (Expr (..), Position (..), Program (..))
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
import ModuleResolver (ModuleResolver, extractImports, resolveModuleFromPath)
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

data ImportError = ImportError {errorMessage :: String, errorPosition :: Position}

checkImportDiagnostics :: ModuleResolver -> [Expr] -> FilePath -> Text -> IO [Diagnostic]
checkImportDiagnostics resolver exprs currentFile sourceText = do
    let imports = extractImports exprs
    importErrors <- concatMapM (checkImport resolver currentFile) imports
    return $ mapMaybe (`importErrorToDiagnostic` sourceText) importErrors
  where
    checkImport resolver' currentFile' (Import{from, importPos}) = do
        maybeModule <- resolveModuleFromPath resolver' from currentFile'
        case maybeModule of
            Just _ -> return []
            Nothing -> return [ImportError{errorMessage = "Module '" ++ from ++ "' not found", errorPosition = importPos}]
    checkImport _ _ _ = return []
    importErrorToDiagnostic :: ImportError -> Text -> Maybe Diagnostic
    importErrorToDiagnostic (ImportError{errorMessage = msg, errorPosition = pos}) srcText = do
        range <- indigoToLspRange pos srcText
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
    concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
    concatMapM f xs = concat <$> mapM f xs

getDiagnostics :: Text -> ModuleResolver -> FilePath -> IO [Diagnostic]
getDiagnostics sourceText resolver filePath = do
    preludeExprs <- getPreludeExprs
    let parseResult = parseProgram sourceText initCompilerFlags
    case parseResult of
        Left parseErrorBundle -> do
            let sourceErrors = parseErrorBundleToSourceErrors parseErrorBundle sourceText
            return $ mapMaybe (`sourceErrorToDiagnostic` sourceText) sourceErrors
        Right (Program userExprs _) -> do
            importDiags <- checkImportDiagnostics resolver userExprs filePath sourceText
            let programWithPrelude = Program (preludeExprs ++ userExprs) Nothing
            compileResult <- evalStateT (compileProgram programWithPrelude) (initCompilerState programWithPrelude)
            case compileResult of
                Left _ -> return importDiags
                Right compilerErrors -> do
                    let compilerDiags = mapMaybe (`compilerErrorToDiagnostic` sourceText) compilerErrors
                    return $ importDiags ++ compilerDiags
