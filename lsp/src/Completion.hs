module Completion where

import AST (Expr (..), Position (..), Program (..), Type (..), children)
import Control.Monad (Monad)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (nub)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import IndigoPrelude (getPreludeExprs)
import Language.LSP.Protocol.Types
    ( CompletionItem (..)
    , CompletionItemKind (..)
    , CompletionList (..)
    , MarkupContent (..)
    , MarkupKind (..)
    )
import Language.LSP.Protocol.Types qualified as LSP
import ModuleResolver (ModuleResolver, extractImports, resolveModuleFromPath)
import Parser (initCompilerFlags, needsMain, parseProgram)
import Position (lspToIndigoOffset)

indigoKeywords :: [String]
indigoKeywords =
    [ "let"
    , "if"
    , "then"
    , "else"
    , "do"
    , "end"
    , "struct"
    , "trait"
    , "impl"
    , "for"
    , "is"
    , "when"
    , "of"
    , "import"
    , "from"
    , "as"
    , "external"
    , "return"
    ]

extractFunctions :: [Expr] -> [String]
extractFunctions exprs =
    nub $
        concatMap extractFromExpr exprs
  where
    extractFromExpr expr = case expr of
        FuncDef{name} -> name : concatMap extractFromExpr (AST.children expr)
        FuncDec{name} -> [name]
        Function{dec} -> case dec of
            FuncDec{name} -> [name]
            _ -> []
        _ -> concatMap extractFromExpr (AST.children expr)

extractStructs :: [Expr] -> [String]
extractStructs exprs =
    nub $
        concatMap extractFromExpr exprs
  where
    extractFromExpr expr = case expr of
        Struct{name} -> [name]
        _ -> concatMap extractFromExpr (AST.children expr)

extractStructDefinitions :: [Expr] -> [(String, [(String, Type)])]
extractStructDefinitions exprs =
    nubBy (\(name1, _) (name2, _) -> name1 == name2) $
        concatMap extractFromExpr exprs
  where
    extractFromExpr expr = case expr of
        Struct{name, fields} -> [(name, fields)]
        _ -> concatMap extractFromExpr (AST.children expr)
    nubBy _ [] = []
    nubBy eq (x : xs) = x : nubBy eq (filter (not . eq x) xs)

extractVariablesRecursive :: Expr -> [(String, AST.Position)]
extractVariablesRecursive expr =
    case expr of
        Let{letName, letPos} -> [(letName, letPos)]
        _ -> []
        ++ concatMap extractVariablesRecursive (children expr)

extractVariables :: [Expr] -> Int -> [String]
extractVariables exprs cursorOffset =
    let allVars = concatMap extractVariablesRecursive exprs
        inScopeVars = filter (\(_, AST.Position (start, _)) -> start < cursorOffset) allVars
     in nub $ map fst inScopeVars

extractVariableTypes :: [Expr] -> Int -> [(String, Maybe Type)]
extractVariableTypes exprs cursorOffset =
    let allVars = concatMap extractVariablesRecursive exprs
        inScopeVars = filter (\(_, AST.Position (start, _)) -> start < cursorOffset) allVars
        varNames = map fst inScopeVars
        varTypes = map (\name -> (name, findVariableType name exprs cursorOffset)) varNames
     in nubBy (\(name1, _) (name2, _) -> name1 == name2) varTypes
  where
    nubBy _ [] = []
    nubBy eq (x : xs) = x : nubBy eq (filter (not . eq x) xs)

findVariableType :: String -> [Expr] -> Int -> Maybe Type
findVariableType varName exprs cursorOffset =
    let findInExpr expr = case expr of
            Let{letName, letValue, letPos = AST.Position (start, _)}
                | letName == varName && start < cursorOffset ->
                    getExprType letValue
            _ -> findInChildren expr
        findInChildren expr = case expr of
            FuncDef{body} -> findInExpr body
            Function{def} -> firstJust (map findInExpr def)
            DoBlock{doBlockExprs} -> firstJust (map findInExpr doBlockExprs)
            _ -> Nothing
        firstJust [] = Nothing
        firstJust (Just x : _) = Just x
        firstJust (Nothing : xs) = firstJust xs
        results = map findInExpr exprs
     in firstJust results

getExprType :: Expr -> Maybe Type
getExprType expr = case expr of
    StructLit{structLitName, structLitTypeArgs} -> Just $ StructT structLitName structLitTypeArgs
    Var{varName} -> Just $ StructT varName []
    _ -> Nothing

isStructFieldAccessContext :: Text -> Int -> Bool
isStructFieldAccessContext sourceText offset =
    (offset > 0 && offset <= T.length sourceText)
        && ( let beforeCursor = T.take offset sourceText
                 trimmed = T.stripEnd beforeCursor
              in not (T.null trimmed) && T.last trimmed == '.'
           )

getStructTypeBeforeDot :: Text -> Int -> [Expr] -> Maybe String
getStructTypeBeforeDot sourceText offset exprs =
    if offset > 0 && offset <= T.length sourceText
        then
            let beforeCursor = T.take offset sourceText
                trimmed = T.stripEnd beforeCursor
             in if not (T.null trimmed) && T.last trimmed == '.'
                    then
                        let exprText = T.init trimmed
                            varName = extractVarNameBeforeDot exprText
                         in case varName of
                                Just name ->
                                    findVariableType name exprs offset >>= \case
                                        StructT structName _ -> Just structName
                                        _ -> Nothing
                                Nothing ->
                                    let parseResult = parseProgram exprText initCompilerFlags{needsMain = False}
                                     in case parseResult of
                                            Right (Program parsedExprs _) ->
                                                case parsedExprs of
                                                    [expr] -> getStructTypeFromExpr expr exprs
                                                    _ -> Nothing
                                            _ -> Nothing
                    else Nothing
        else Nothing

extractVarNameBeforeDot :: Text -> Maybe String
extractVarNameBeforeDot text =
    let reversed = T.reverse text
        isIdentifierChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_' || c == '\''
        takeWhileIdentifier = T.takeWhile isIdentifierChar
        identifier = takeWhileIdentifier reversed
     in if T.null identifier
            then Nothing
            else Just $ T.unpack $ T.reverse identifier

getStructTypeFromExpr :: Expr -> [Expr] -> Maybe String
getStructTypeFromExpr expr allExprs = case expr of
    StructLit{structLitName} -> Just structLitName
    Var{varName} ->
        findVariableType varName allExprs 100000 >>= \case
            StructT name _ -> Just name
            _ -> Nothing
    StructAccess{structAccessStruct} -> getStructTypeFromExpr structAccessStruct allExprs
    _ -> Nothing

getStructFieldCompletions :: String -> [(String, [(String, Type)])] -> [CompletionItem]
getStructFieldCompletions structName structDefs =
    case lookup structName structDefs of
        Just fields ->
            map
                ( \(fieldName, fieldType) ->
                    CompletionItem
                        { _label = T.pack fieldName
                        , _labelDetails = Nothing
                        , _kind = Just CompletionItemKind_Field
                        , _tags = Nothing
                        , _detail = Just $ T.pack $ "Field: " ++ fieldName ++ " : " ++ show fieldType
                        , _documentation = Nothing
                        , _deprecated = Nothing
                        , _preselect = Nothing
                        , _sortText = Just $ T.pack ("0" ++ fieldName)
                        , _filterText = Nothing
                        , _insertText = Just $ T.pack fieldName
                        , _insertTextFormat = Nothing
                        , _insertTextMode = Nothing
                        , _textEdit = Nothing
                        , _textEditText = Nothing
                        , _additionalTextEdits = Nothing
                        , _commitCharacters = Nothing
                        , _command = Nothing
                        , _data_ = Nothing
                        }
                )
                fields
        Nothing -> []

tryParseWithFallback :: Text -> Either String (Program, [Expr])
tryParseWithFallback sourceText =
    let flags = initCompilerFlags{needsMain = False}
     in case parseProgram sourceText flags of
            Right (Program exprs _) -> Right (Program exprs Nothing, exprs)
            Left _ ->
                let sourceLines = T.lines sourceText
                    completeLines = filter (not . isIncompleteLine) sourceLines
                    truncatedSource = T.unlines completeLines
                 in case parseProgram truncatedSource flags of
                        Right (Program exprs _) -> Right (Program exprs Nothing, exprs)
                        Left _ -> Right (Program [] Nothing, [])
  where
    isIncompleteLine line =
        let trimmed = T.stripEnd line
         in not (T.null trimmed) && T.last trimmed `elem` ['=', '.']

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

extractImportedSymbols :: ModuleResolver -> [Expr] -> FilePath -> IO ([String], [String], [String])
extractImportedSymbols resolver exprs currentFile = do
    let imports = extractImports exprs
    importedFunctions <- concatMapM (extractFromImport resolver currentFile extractFunctionsFromProgram) imports
    importedStructs <- concatMapM (extractFromImport resolver currentFile extractStructsFromProgram) imports
    importedVars <- concatMapM (extractFromImport resolver currentFile extractVariablesFromProgram) imports
    return (nub importedFunctions, nub importedStructs, nub importedVars)
  where
    extractFromImport :: ModuleResolver -> FilePath -> (Program -> [String]) -> Expr -> IO [String]
    extractFromImport resolver' currentFile' extractor (Import{from, qualified, as}) = do
        maybeModule <- resolveModuleFromPath resolver' from currentFile'
        case maybeModule of
            Just (prog, _) -> do
                let symbols = extractor prog
                let prefix = if qualified then fromMaybe from as else ""
                return $ if null prefix then symbols else map ((prefix ++ ".") ++) symbols
            Nothing -> return []
    extractFromImport _ _ _ _ = return []
    extractFunctionsFromProgram :: Program -> [String]
    extractFunctionsFromProgram (Program exprs' _) = extractFunctions exprs'
    extractStructsFromProgram :: Program -> [String]
    extractStructsFromProgram (Program exprs' _) = extractStructs exprs'
    extractVariablesFromProgram :: Program -> [String]
    extractVariablesFromProgram (Program exprs' _) = map fst $ concatMap extractVariablesRecursive exprs'

getCompletionsForPosition :: (MonadIO m) => Text -> LSP.Position -> ModuleResolver -> FilePath -> m (Either () CompletionList)
getCompletionsForPosition sourceText pos resolver filePath = do
    preludeExprs <- getPreludeExprs
    let offset = lspToIndigoOffset pos sourceText
        parseResult = tryParseWithFallback sourceText
    case parseResult of
        Left _ -> return $ Right $ CompletionList False Nothing []
        Right (_, exprs) -> do
            (importedFuncs, importedStructs, importedVars) <- liftIO $ extractImportedSymbols resolver exprs filePath
            let allExprs = preludeExprs ++ exprs
                functions = extractFunctions allExprs ++ importedFuncs
                structs = extractStructs allExprs ++ importedStructs
                variables = extractVariables exprs offset ++ importedVars
                structDefs = extractStructDefinitions allExprs
                isFieldAccess = isStructFieldAccessContext sourceText offset
                structType = if isFieldAccess then getStructTypeBeforeDot sourceText offset allExprs else Nothing
                fieldItems = case structType of
                    Just structName -> getStructFieldCompletions structName structDefs
                    Nothing -> []
                keywordItems =
                    map
                        ( \kw ->
                            CompletionItem
                                { _label = T.pack kw
                                , _labelDetails = Nothing
                                , _kind = Just CompletionItemKind_Keyword
                                , _tags = Nothing
                                , _detail = Nothing
                                , _documentation = Nothing
                                , _deprecated = Nothing
                                , _preselect = Nothing
                                , _sortText = Just $ T.pack ("1" ++ kw)
                                , _filterText = Nothing
                                , _insertText = Just $ T.pack kw
                                , _insertTextFormat = Nothing
                                , _insertTextMode = Nothing
                                , _textEdit = Nothing
                                , _textEditText = Nothing
                                , _additionalTextEdits = Nothing
                                , _commitCharacters = Nothing
                                , _command = Nothing
                                , _data_ = Nothing
                                }
                        )
                        indigoKeywords
                functionItems =
                    map
                        ( \fn ->
                            CompletionItem
                                { _label = T.pack fn
                                , _labelDetails = Nothing
                                , _kind = Just CompletionItemKind_Function
                                , _tags = Nothing
                                , _detail = Just $ T.pack ("Function: " ++ fn)
                                , _documentation = Nothing
                                , _deprecated = Nothing
                                , _preselect = Nothing
                                , _sortText = Just $ T.pack ("2" ++ fn)
                                , _filterText = Nothing
                                , _insertText = Just $ T.pack fn
                                , _insertTextFormat = Nothing
                                , _insertTextMode = Nothing
                                , _textEdit = Nothing
                                , _textEditText = Nothing
                                , _additionalTextEdits = Nothing
                                , _commitCharacters = Nothing
                                , _command = Nothing
                                , _data_ = Nothing
                                }
                        )
                        functions
                structItems =
                    map
                        ( \st ->
                            CompletionItem
                                { _label = T.pack st
                                , _labelDetails = Nothing
                                , _kind = Just CompletionItemKind_Struct
                                , _tags = Nothing
                                , _detail = Just $ T.pack ("Struct: " ++ st)
                                , _documentation = Nothing
                                , _deprecated = Nothing
                                , _preselect = Nothing
                                , _sortText = Just $ T.pack ("3" ++ st)
                                , _filterText = Nothing
                                , _insertText = Just $ T.pack st
                                , _insertTextFormat = Nothing
                                , _insertTextMode = Nothing
                                , _textEdit = Nothing
                                , _textEditText = Nothing
                                , _additionalTextEdits = Nothing
                                , _commitCharacters = Nothing
                                , _command = Nothing
                                , _data_ = Nothing
                                }
                        )
                        structs
                variableItems =
                    map
                        ( \var ->
                            CompletionItem
                                { _label = T.pack var
                                , _labelDetails = Nothing
                                , _kind = Just CompletionItemKind_Variable
                                , _tags = Nothing
                                , _detail = Just $ T.pack ("Variable: " ++ var)
                                , _documentation = Nothing
                                , _deprecated = Nothing
                                , _preselect = Nothing
                                , _sortText = Just $ T.pack ("4" ++ var)
                                , _filterText = Nothing
                                , _insertText = Just $ T.pack var
                                , _insertTextFormat = Nothing
                                , _insertTextMode = Nothing
                                , _textEdit = Nothing
                                , _textEditText = Nothing
                                , _additionalTextEdits = Nothing
                                , _commitCharacters = Nothing
                                , _command = Nothing
                                , _data_ = Nothing
                                }
                        )
                        variables
                allItems =
                    if isFieldAccess && not (null fieldItems)
                        then fieldItems
                        else keywordItems ++ functionItems ++ structItems ++ variableItems
            return $ Right $ CompletionList False Nothing allItems

resolveCompletionItem :: CompletionItem -> CompletionItem
resolveCompletionItem item@CompletionItem{_label = labelText, _kind = kind, _documentation = existingDoc} =
    let label = T.unpack labelText
        mkDoc content = Just $ LSP.InR (MarkupContent MarkupKind_Markdown content)
        updateDoc doc = item{_documentation = doc} :: CompletionItem
     in case kind of
            Just CompletionItemKind_Keyword ->
                updateDoc $ mkDoc $ getKeywordDocumentation label
            Just CompletionItemKind_Function ->
                updateDoc $ mkDoc $ T.pack $ "Function: `" ++ label ++ "`\n\nUse this function in your Indigo code."
            Just CompletionItemKind_Struct ->
                updateDoc $ mkDoc $ T.pack $ "Struct: `" ++ label ++ "`\n\nA structure type in Indigo."
            Just CompletionItemKind_Variable ->
                updateDoc $ mkDoc $ T.pack $ "Variable: `" ++ label ++ "`\n\nA variable binding in the current scope."
            Just CompletionItemKind_Field ->
                updateDoc $ mkDoc $ T.pack $ "Field: `" ++ label ++ "`\n\nA struct field."
            _ -> case existingDoc of
                Nothing -> updateDoc $ mkDoc $ T.pack $ "`" ++ label ++ "`"
                Just _ -> item

getKeywordDocumentation :: String -> Text
getKeywordDocumentation "let" = T.pack "**Let binding**\n\nBinds a value to a name.\n\n```indigo\nlet x = 42\n```"
getKeywordDocumentation "if" = T.pack "**If expression**\n\nConditional expression.\n\n```indigo\nif condition then expr1 else expr2\n```"
getKeywordDocumentation "then" = T.pack "**Then clause**\n\nPart of an if expression."
getKeywordDocumentation "else" = T.pack "**Else clause**\n\nPart of an if expression."
getKeywordDocumentation "do" = T.pack "**Do block**\n\nSequential execution block.\n\n```indigo\ndo\n  expr1\n  expr2\nend\n```"
getKeywordDocumentation "end" = T.pack "**End keyword**\n\nCloses a do block or other block structure."
getKeywordDocumentation "struct" = T.pack "**Struct definition**\n\nDefines a structure type.\n\n```indigo\nstruct Person = (name: String, age: Int)\n```"
getKeywordDocumentation "trait" = T.pack "**Trait definition**\n\nDefines a trait (interface).\n\n```indigo\ntrait Monad = do\n  bind :: Self -> (Any -> Self) -> Self\nend\n```"
getKeywordDocumentation "impl" = T.pack "**Implementation**\n\nImplements a trait for a type.\n\n```indigo\nimpl Monad for IO = do\n  bind IO{inner: x} f = f x\nend\n```"
getKeywordDocumentation "for" = T.pack "**For keyword**\n\nUsed in trait implementations."
getKeywordDocumentation "is" = T.pack "**Is keyword**\n\nUsed to specify trait implementations in struct definitions."
getKeywordDocumentation "when" = T.pack "**When expression**\n\nPattern matching expression.\n\n```indigo\nwhen value of\n  Pattern1 -> result1\n  Pattern2 -> result2\n  else -> default\nend\n```"
getKeywordDocumentation "of" = T.pack "**Of keyword**\n\nPart of a when expression."
getKeywordDocumentation "import" = T.pack "**Import statement**\n\nImports modules.\n\n```indigo\nimport ModuleName\n```"
getKeywordDocumentation "from" = T.pack "**From keyword**\n\nUsed in import statements."
getKeywordDocumentation "as" = T.pack "**As keyword**\n\nUsed for type casting or aliasing.\n\n```indigo\nx as Type\n```"
getKeywordDocumentation "external" = T.pack "**External declaration**\n\nDeclares external functions for FFI."
getKeywordDocumentation "return" = T.pack "**Return keyword**\n\nReturns a value from a function."
getKeywordDocumentation _ = T.pack "Indigo keyword"
