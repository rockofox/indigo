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
        errorLines = zip [1 ..] $ map (\SourceError{errorMessage = msg, errorPosition = pos} -> (pos, msg)) errors
     in unlines $ concatMap (renderSingleError linesOfInput) errorLines

renderSingleError :: [String] -> (Int, (Position, String)) -> [String]
renderSingleError linesOfInput (errorNum, (Position (start, end), msg)) =
    let (startLine, startColumn) = getLineAndColumn start linesOfInput
        (endLine, endColumn) = getLineAndColumn end linesOfInput
        totalLines = length linesOfInput
        contextBefore = max 1 (startLine - 2)
        contextAfter = min totalLines (endLine + 2)
        hasContext = contextBefore < startLine || endLine < contextAfter - 1
        maxLineNumWidth = length (show (max startLine endLine))
        (beforeLines, errorLines, afterLines) = splitContextLines contextBefore startLine endLine contextAfter
     in renderErrorHeader errorNum msg
            ++ (if hasContext then renderErrorLocation startLine startColumn else [])
            ++ renderContextLines linesOfInput beforeLines maxLineNumWidth
            ++ renderErrorLines linesOfInput errorLines maxLineNumWidth
            ++ renderErrorUnderline startLine startColumn endLine endColumn linesOfInput maxLineNumWidth
            ++ renderContextLines linesOfInput afterLines maxLineNumWidth
            ++ (["  \x1b[36m└──\x1b[0m" | hasContext])
            ++ [""]

splitContextLines :: Int -> Int -> Int -> Int -> ([Int], [Int], [Int])
splitContextLines contextBefore startLine endLine contextAfter =
    let allLines = [contextBefore .. contextAfter]
        before = filter (< startLine) allLines
        errorLines = filter (\n -> n >= startLine && n <= endLine) allLines
        after = filter (> endLine) allLines
     in (before, errorLines, after)

renderErrorHeader :: Int -> String -> [String]
renderErrorHeader errorNum msg =
    ["\x1b[1m\x1b[31merror\x1b[0m\x1b[1m[" ++ show errorNum ++ "]\x1b[0m: " ++ msg]

renderErrorLocation :: Int -> Int -> [String]
renderErrorLocation startLine startColumn =
    ["  \x1b[36m┌──\x1b[0m \x1b[2m" ++ show startLine ++ ":" ++ show startColumn ++ "\x1b[0m"]

renderContextLines :: [String] -> [Int] -> Int -> [String]
renderContextLines linesOfInput lineNums maxLineNumWidth =
    concatMap
        ( \lineNum ->
            if lineNum >= 1 && lineNum <= length linesOfInput
                then
                    let lineContent = linesOfInput !! (lineNum - 1)
                        lineNumStr = show lineNum
                        padding = replicate (maxLineNumWidth - length lineNumStr) ' '
                        lineNumColor = "\x1b[2m"
                        lineNumReset = "\x1b[0m"
                     in ["  \x1b[36m│\x1b[0m" ++ padding ++ lineNumColor ++ lineNumStr ++ lineNumReset ++ " \x1b[36m│\x1b[0m " ++ lineContent]
                else []
        )
        lineNums

renderErrorLines :: [String] -> [Int] -> Int -> [String]
renderErrorLines linesOfInput errorLineNums maxLineNumWidth =
    concatMap
        ( \lineNum ->
            if lineNum >= 1 && lineNum <= length linesOfInput
                then
                    let lineContent = linesOfInput !! (lineNum - 1)
                        lineNumStr = show lineNum
                        padding = replicate (maxLineNumWidth - length lineNumStr) ' '
                        lineNumColor = "\x1b[1m\x1b[31m"
                        lineNumReset = "\x1b[0m"
                     in ["  \x1b[36m│\x1b[0m" ++ padding ++ lineNumColor ++ lineNumStr ++ lineNumReset ++ " \x1b[36m│\x1b[0m " ++ lineContent]
                else []
        )
        errorLineNums

renderErrorUnderline :: Int -> Int -> Int -> Int -> [String] -> Int -> [String]
renderErrorUnderline startLine startColumn endLine endColumn linesOfInput maxLineNumWidth =
    if startLine >= 1 && startLine <= length linesOfInput
        then
            let lineContent = linesOfInput !! (startLine - 1)
                lineLength = length lineContent
                underlineStartCol = max 0 (min startColumn lineLength)
                underlineEndCol = if startLine == endLine then max underlineStartCol (min endColumn lineLength) else underlineStartCol + 1
                underlineLength = max 1 (underlineEndCol - underlineStartCol)
                lineNumStr = show startLine
                padding = replicate (maxLineNumWidth - length lineNumStr) ' '
                spacesBefore = replicate underlineStartCol ' '
                underlineWithCaret =
                    if underlineLength <= 1
                        then "\x1b[1m\x1b[31m^\x1b[0m"
                        else
                            let squiggles = take underlineLength $ cycle "~"
                             in "\x1b[1m\x1b[31m" ++ squiggles ++ "\x1b[0m"
             in ["  \x1b[36m│\x1b[0m" ++ padding ++ replicate (length lineNumStr) ' ' ++ " \x1b[36m│\x1b[0m " ++ spacesBefore ++ underlineWithCaret]
        else []

getLineAndColumn :: Int -> [String] -> (Int, Int)
getLineAndColumn charPos linesOfInput =
    let go _ _ [] = (0, 0)
        go remaining pos (l : ls)
            | remaining <= length l = (pos, remaining)
            | otherwise = go (remaining - length l - 1) (pos + 1) ls
     in go charPos 1 linesOfInput
