module ErrorRenderer (SourceError (..), renderErrors, parseErrorBundleToSourceErrors) where

import AST (Position (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
    ( ParseError (..)
    , ParseErrorBundle (..)
    , errorOffset
    , parseErrorTextPretty
    )

data SourceError = SourceError
    { errorMessage :: String
    , errorPosition :: Position
    }
    deriving (Show, Eq)

parseErrorBundleToSourceErrors :: ParseErrorBundle Text Void -> Text -> [SourceError]
parseErrorBundleToSourceErrors bundle _sourceText =
    let errors = bundleErrors bundle
        primaryError = NonEmpty.head errors
     in [parseErrorToSourceError primaryError]

parseErrorToSourceError :: ParseError Text Void -> SourceError
parseErrorToSourceError err =
    let offset = errorOffset err
        errorMsg = parseErrorTextPretty err
        (start, end) = (offset, offset)
        position = Position (start, end)
     in SourceError{errorMessage = errorMsg, errorPosition = position}

renderErrors :: [SourceError] -> String -> String
renderErrors errors input =
    let linesOfInput = lines input
        errorLines = map (\SourceError{errorMessage = msg, errorPosition = pos} -> (pos, msg)) errors
     in unlines $
            map
                ( \(Position (start, end), msg) ->
                    let (startLine, startColumn) = getLineAndColumn start linesOfInput
                        (endLine, endColumn) = getLineAndColumn end linesOfInput
                        lineContent =
                            if startLine <= 0 || startLine > length linesOfInput
                                then "Line " ++ show startLine ++ " out of range"
                                else linesOfInput !! (startLine - 1)
                        lineIndicator =
                            replicate startColumn ' '
                                ++ ( if startLine == endLine
                                        then "\x1b[31m  └" ++ replicate (endColumn - startColumn) '─' ++ "┘\x1b[0m"
                                        else "\x1b[31m  └" ++ replicate (length lineContent - startColumn) '─' ++ "┘\x1b[0m"
                                   )
                     in "\x1b[33m"
                            ++ show startLine
                            ++ "\x1b[0m:\x1b[91m "
                            ++ lineContent
                            ++ "\x1b[0m\n"
                            ++ lineIndicator
                            ++ " "
                            ++ "\x1b[31m"
                            ++ msg
                            ++ "\x1b[0m"
                )
                errorLines

getLineAndColumn :: Int -> [String] -> (Int, Int)
getLineAndColumn charPos linesOfInput =
    let go _ _ [] = (0, 0)
        go remaining pos (l : ls)
            | remaining <= length l = (pos, remaining)
            | otherwise = go (remaining - length l - 1) (pos + 1) ls
     in go charPos 1 linesOfInput
