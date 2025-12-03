module Hover where

import AST qualified
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (listToMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import IndigoPrelude (getPreludeExprs)
import Language.LSP.Protocol.Types (Hover (..), MarkupContent (..), MarkupKind (..), Position)
import Language.LSP.Protocol.Types qualified as LSPTypes
import ModuleResolver
import Parser (initCompilerFlags, needsMain, parseProgram)
import Position (lspToIndigoOffset)

findExprAtPosition :: AST.Expr -> Int -> Maybe (AST.Expr, AST.Position)
findExprAtPosition expr offset =
    let exprPos = getExprPosition expr
        (AST.Position (start, end)) = exprPos
     in if offset >= start && offset <= end
            then
                let childMatches = concatMap (\child -> maybeToList (findExprAtPosition child offset)) (getExprChildren expr)
                 in case childMatches of
                        [] -> Just (expr, exprPos)
                        (match : _) -> Just match
            else Nothing

getExprPosition :: AST.Expr -> AST.Position
getExprPosition (AST.Var{varPos}) = varPos
getExprPosition (AST.FuncCall{funcPos}) = funcPos
getExprPosition (AST.BoolLit{boolPos}) = boolPos
getExprPosition (AST.IntLit{intPos}) = intPos
getExprPosition (AST.StringLit{stringPos}) = stringPos
getExprPosition (AST.FloatLit{floatPos}) = floatPos
getExprPosition (AST.DoubleLit{doublePos}) = doublePos
getExprPosition (AST.CharLit{charPos}) = charPos
getExprPosition (AST.If{ifPos}) = ifPos
getExprPosition (AST.Let{letPos}) = letPos
getExprPosition (AST.FuncDef{funcDefPos}) = funcDefPos
getExprPosition (AST.FuncDec{funcDecPos}) = funcDecPos
getExprPosition (AST.Function{functionPos}) = functionPos
getExprPosition (AST.Add{addPos}) = addPos
getExprPosition (AST.Sub{subPos}) = subPos
getExprPosition (AST.Mul{mulPos}) = mulPos
getExprPosition (AST.Div{divPos}) = divPos
getExprPosition (AST.Eq{eqPos}) = eqPos
getExprPosition (AST.Neq{neqPos}) = neqPos
getExprPosition (AST.Lt{ltPos}) = ltPos
getExprPosition (AST.Gt{gtPos}) = gtPos
getExprPosition (AST.Le{lePos}) = lePos
getExprPosition (AST.Ge{gePos}) = gePos
getExprPosition (AST.And{andPos}) = andPos
getExprPosition (AST.Or{orPos}) = orPos
getExprPosition (AST.Not{notPos}) = notPos
getExprPosition (AST.Struct{structPos}) = structPos
getExprPosition (AST.StructLit{structLitPos}) = structLitPos
getExprPosition (AST.StructAccess{structAccessPos}) = structAccessPos
getExprPosition (AST.ListLit{listLitPos}) = listLitPos
getExprPosition (AST.ListConcat{listConcatPos}) = listConcatPos
getExprPosition (AST.ListAdd{listAddPos}) = listAddPos
getExprPosition (AST.ArrayAccess{arrayAccessPos}) = arrayAccessPos
getExprPosition (AST.Modulo{moduloPos}) = moduloPos
getExprPosition (AST.Power{powerPos}) = powerPos
getExprPosition (AST.Pipeline{pipelinePos}) = pipelinePos
getExprPosition (AST.Lambda{lambdaPos}) = lambdaPos
getExprPosition (AST.Cast{castPos}) = castPos
getExprPosition (AST.When{whenPos}) = whenPos
getExprPosition (AST.TupleLit{tupleLitPos}) = tupleLitPos
getExprPosition (AST.TupleAccess{tupleAccessPos}) = tupleAccessPos
getExprPosition _ = AST.Position (-1, -1)

getExprChildren :: AST.Expr -> [AST.Expr]
getExprChildren (AST.Add a b _) = [a, b]
getExprChildren (AST.Sub a b _) = [a, b]
getExprChildren (AST.Mul a b _) = [a, b]
getExprChildren (AST.Div a b _) = [a, b]
getExprChildren (AST.Eq a b _) = [a, b]
getExprChildren (AST.Neq a b _) = [a, b]
getExprChildren (AST.Lt a b _) = [a, b]
getExprChildren (AST.Gt a b _) = [a, b]
getExprChildren (AST.Le a b _) = [a, b]
getExprChildren (AST.Ge a b _) = [a, b]
getExprChildren (AST.And a b _) = [a, b]
getExprChildren (AST.Or a b _) = [a, b]
getExprChildren (AST.Not a _) = [a]
getExprChildren (AST.If a b c _) = [a, b, c]
getExprChildren (AST.Let _ a _) = [a]
getExprChildren (AST.FuncDef{body}) = [body]
getExprChildren (AST.Function{def, dec}) = dec : def
getExprChildren (AST.FuncCall{funcArgs}) = funcArgs
getExprChildren (AST.DoBlock a _) = a
getExprChildren (AST.StructLit{structLitFields}) = map snd structLitFields
getExprChildren (AST.StructAccess a b _) = [a, b]
getExprChildren (AST.ListLit a _) = a
getExprChildren (AST.ListConcat a b _) = [a, b]
getExprChildren (AST.ListAdd a b _) = [a, b]
getExprChildren (AST.ArrayAccess a b _) = [a, b]
getExprChildren (AST.Modulo a b _) = [a, b]
getExprChildren (AST.Power a b _) = [a, b]
getExprChildren (AST.Pipeline a b _) = [a, b]
getExprChildren (AST.Lambda _ a _) = [a]
getExprChildren (AST.Cast a b _) = [a, b]
getExprChildren (AST.When expr branches else_ _) = expr : concatMap (\(p, b) -> [p, b]) branches ++ maybeToList else_
getExprChildren (AST.TupleLit a _) = a
getExprChildren (AST.TupleAccess a _ _) = [a]
getExprChildren _ = []

getHoverForExpr :: AST.Expr -> Text -> Maybe Hover
getHoverForExpr expr _sourceText =
    let mkHover content = Just $ Hover (LSPTypes.InL (MarkupContent MarkupKind_Markdown content)) Nothing
     in case expr of
            AST.Var{varName} -> mkHover $ T.pack $ "**Variable:** `" ++ varName ++ "`"
            AST.FuncCall{funcName} -> mkHover $ T.pack $ "**Function call:** `" ++ funcName ++ "`"
            AST.FuncDef{name} -> mkHover $ T.pack $ "**Function definition:** `" ++ name ++ "`"
            AST.FuncDec{name, types} ->
                let typeStr = showTypes types
                 in mkHover $ T.pack $ "**Function declaration:** `" ++ name ++ " :: " ++ typeStr ++ "`"
            AST.Function{dec = AST.FuncDec{name, types}} ->
                let typeStr = showTypes types
                 in mkHover $ T.pack $ "**Function:** `" ++ name ++ " :: " ++ typeStr ++ "`"
            AST.StructLit{structLitName} -> mkHover $ T.pack $ "**Struct literal:** `" ++ structLitName ++ "`"
            AST.Struct{name} -> mkHover $ T.pack $ "**Struct definition:** `" ++ name ++ "`"
            AST.Let{letName} -> mkHover $ T.pack $ "**Let binding:** `" ++ letName ++ "`"
            AST.BoolLit{boolValue} -> mkHover $ T.pack $ "**Boolean literal:** `" ++ show boolValue ++ "` (Bool)"
            AST.IntLit{intValue} -> mkHover $ T.pack $ "**Integer literal:** `" ++ show intValue ++ "` (Int)"
            AST.StringLit{stringValue} -> mkHover $ T.pack $ "**String literal:** `" ++ show stringValue ++ "` (String)"
            AST.FloatLit{floatValue} -> mkHover $ T.pack $ "**Float literal:** `" ++ show floatValue ++ "` (Float)"
            AST.DoubleLit{doubleValue} -> mkHover $ T.pack $ "**Double literal:** `" ++ show doubleValue ++ "` (Double)"
            AST.CharLit{charValue} -> mkHover $ T.pack $ "**Char literal:** `" ++ show charValue ++ "` (Char)"
            AST.ListLit{} -> mkHover $ T.pack "**List literal**"
            AST.TupleLit{} -> mkHover $ T.pack "**Tuple literal**"
            AST.Lambda{} -> mkHover $ T.pack "**Lambda expression**"
            AST.If{} -> mkHover $ T.pack "**If expression**"
            AST.When{} -> mkHover $ T.pack "**When expression**"
            AST.DoBlock{} -> mkHover $ T.pack "**Do block**"
            _ -> Nothing

showTypes :: [AST.Type] -> String
showTypes [] = ""
showTypes [t] = show t
showTypes (t : ts) = show t ++ " -> " ++ showTypes ts

findPreludeDefinition :: [AST.Expr] -> String -> Maybe AST.Expr
findPreludeDefinition preludeExprs name =
    listToMaybe $
        concatMap
            ( \expr -> case expr of
                AST.FuncDef{name = fnName} | fnName == name -> [expr]
                AST.FuncDec{name = fnName} | fnName == name -> [expr]
                AST.Struct{name = structName} | structName == name -> [expr]
                _ -> []
            )
            preludeExprs

resolveQualifiedSymbol :: String -> (String, String)
resolveQualifiedSymbol name =
    let parts = T.splitOn (T.pack ".") (T.pack name)
     in if length parts > 1
            then (T.unpack $ head parts, T.unpack $ T.intercalate (T.pack ".") $ tail parts)
            else ("", name)

findQualifiedAccessAtOffset :: Int -> AST.Expr -> Maybe (String, String)
findQualifiedAccessAtOffset offset expr =
    case expr of
        AST.StructAccess{structAccessStruct = AST.Var{varName = moduleName}, structAccessField = AST.Var{varName = symbolName, varPos = fieldPos}} ->
            let (AST.Position (fieldStart, fieldEnd)) = fieldPos
             in if offset >= fieldStart && offset <= fieldEnd
                    then Just (moduleName, symbolName)
                    else Nothing
        _ ->
            listToMaybe $ mapMaybe (findQualifiedAccessAtOffset offset) (getExprChildren expr)

getHover :: (MonadIO m) => Text -> Position -> ModuleResolver -> FilePath -> m (Maybe Hover)
getHover sourceText pos resolver filePath = do
    preludeExprs <- getPreludeExprs
    let offset = lspToIndigoOffset pos sourceText
    parseResult <- case parseProgram sourceText initCompilerFlags{needsMain = False} of
        Left _ -> return Nothing
        Right program -> return $ Just (AST.exprs program)
    case parseResult of
        Nothing -> return Nothing
        Just userExprs -> do
            let exprMatch = listToMaybe $ concatMap (\e -> maybeToList (findExprAtPosition e offset)) userExprs
            let qualifiedMatch = listToMaybe $ mapMaybe (findQualifiedAccessAtOffset offset) userExprs
            case exprMatch of
                Nothing -> return Nothing
                Just (expr, exprPos) -> do
                    case qualifiedMatch of
                        Just (moduleName, symbolName) -> do
                            maybeModule <- liftIO $ resolveModuleFromPath resolver moduleName filePath
                            case maybeModule of
                                Just (prog, _) -> do
                                    let (AST.Program exprs' _) = prog
                                    case findDefinitionInModule exprs' symbolName of
                                        Just def -> return $ getHoverForExpr def sourceText
                                        Nothing -> return Nothing
                                Nothing -> return Nothing
                        Nothing -> do
                            let userHover = getHoverForExpr expr sourceText
                            case userHover of
                                Just h -> return $ Just h
                                Nothing -> case expr of
                                    AST.Var{varName} -> do
                                        let (moduleName, symbolName) = resolveQualifiedSymbol varName
                                        if null moduleName
                                            then do
                                                case findPreludeDefinition preludeExprs varName of
                                                    Nothing -> return Nothing
                                                    Just preludeExpr -> return $ getHoverForExpr preludeExpr sourceText
                                            else do
                                                maybeModule <- liftIO $ resolveModuleFromPath resolver moduleName filePath
                                                case maybeModule of
                                                    Just (prog, _) -> do
                                                        let (AST.Program exprs' _) = prog
                                                        case findDefinitionInModule exprs' symbolName of
                                                            Just def -> return $ getHoverForExpr def sourceText
                                                            Nothing -> return Nothing
                                                    Nothing -> return Nothing
                                    AST.FuncCall{funcName} -> do
                                        let (moduleName, symbolName) = resolveQualifiedSymbol funcName
                                        if null moduleName
                                            then do
                                                case findPreludeDefinition preludeExprs funcName of
                                                    Nothing -> return Nothing
                                                    Just preludeExpr -> return $ getHoverForExpr preludeExpr sourceText
                                            else do
                                                maybeModule <- liftIO $ resolveModuleFromPath resolver moduleName filePath
                                                case maybeModule of
                                                    Just (prog, _) -> do
                                                        let (AST.Program exprs' _) = prog
                                                        case findDefinitionInModule exprs' symbolName of
                                                            Just def -> return $ getHoverForExpr def sourceText
                                                            Nothing -> return Nothing
                                                    Nothing -> return Nothing
                                    _ -> return Nothing

findDefinitionInModule :: [AST.Expr] -> String -> Maybe AST.Expr
findDefinitionInModule exprs name = listToMaybe $ concatMap findInExpr exprs
  where
    findInExpr expr = case expr of
        AST.FuncDef{name = fnName} | fnName == name -> [expr]
        AST.FuncDec{name = fnName} | fnName == name -> [expr]
        AST.Function{dec = AST.FuncDec{name = fnName}} | fnName == name -> [expr]
        AST.Struct{name = structName} | structName == name -> [expr]
        AST.Let{letName} | letName == name -> [expr]
        _ -> []
