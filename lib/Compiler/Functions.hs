module Compiler.Functions where

import AST (Position (..), anyPosition, zeroPosition)
import AST qualified as Parser
import Compiler.ControlFlow (compilePatternMatch)
import Compiler.State
    ( CompileExpr
    , Compiler
    , CompilerState (..)
    , External (..)
    , Function (..)
    , Let (..)
    , allocId
    , cerror
    , concatMapM
    , contextStack
    , getErrorCount
    , implsFor
    , internalError
    , structNameFromType
    )
import Compiler.Types
    ( evaluateFunction
    , functionTypesAcceptable
    , getStructFields
    , safeTypeOf
    , typeCompatible
    , typeOf
    , typeToData
    , typeToString
    )
import Control.Monad (forM_, unless, when, zipWithM)
import Control.Monad.State (gets, modify)
import Data.Bifunctor (second)
import Data.Functor ((<&>))
import Data.List (find, inits, intercalate, isInfixOf)
import Data.List.Split qualified
import Data.Map qualified
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.String
import Data.Text (isPrefixOf, splitOn)
import Data.Text qualified
import Data.Text qualified as T
import Foreign (nullPtr, ptrToWordPtr)
import Util (firstJust)
import VM (Action (..), Data (..), Instruction (..))

findAnyFunction :: String -> [Function] -> Maybe Function
findAnyFunction funcName xs = do
    -- TODO: I don't like this function
    let candidates = filter (\y -> baseName y == funcName && funame y /= "main") xs

    let ids = map ((!! 1) . splitOn "#" . Data.Text.pack . funame) candidates
    let ids' = map (read . Data.Text.unpack) ids :: [Int]
    let minId = minimum ids'
    find (\y -> funame y == funcName ++ "#" ++ show minId) candidates

findFunction :: String -> [Function] -> [Parser.Type] -> Maybe Function
findFunction funcName xs typess = do
    -- TODO: I don't like this function
    let candidates = filter (\y -> baseName y == funcName && funame y /= "main" && typesMatch y typess) xs
    let candidates' = case filter (`typesMatchExactly` typess) candidates of
            [] -> candidates
            candidates'' -> candidates''
    let ids = map ((!! 1) . splitOn "#" . Data.Text.pack . funame) candidates'
    let ids' = map (read . Data.Text.unpack) ids :: [Int]
    let minId = minimum ids'

    case find (\y -> funame y == funcName ++ "#" ++ show minId) candidates' of
        Just x -> Just x
        Nothing -> findAnyFunction funcName xs

typesMatch :: Function -> [Parser.Type] -> Bool
typesMatch fun typess = all (uncurry Parser.compareTypes) (zip fun.types typess) && length typess <= length fun.types

typesMatchExactly :: Function -> [Parser.Type] -> Bool
typesMatchExactly fun typess = all (uncurry (==)) (zip fun.types typess) && length typess <= length fun.types

internalFunctions :: [String]
internalFunctions = ["unsafePrint", "unsafeGetLine", "unsafeGetChar", "unsafeRandom", "abs", "root", "sqrt"]

unmangleFunctionName :: String -> String
unmangleFunctionName = takeWhile (/= '#')

extractPosition :: Parser.Expr -> AST.Position
extractPosition (Parser.Var{varPos}) = varPos
extractPosition (Parser.FuncCall{funcPos}) = funcPos
extractPosition (Parser.StructLit{structLitPos}) = structLitPos
extractPosition (Parser.ParenApply{parenApplyPos}) = parenApplyPos
extractPosition (Parser.BoolLit{boolPos}) = boolPos
extractPosition (Parser.IntLit{intPos}) = intPos
extractPosition (Parser.StringLit{stringPos}) = stringPos
extractPosition (Parser.FloatLit{floatPos}) = floatPos
extractPosition (Parser.DoubleLit{doublePos}) = doublePos
extractPosition (Parser.CharLit{charPos}) = charPos
extractPosition (Parser.If{ifPos}) = ifPos
extractPosition (Parser.Let{letPos}) = letPos
extractPosition (Parser.FuncDef{funcDefPos}) = funcDefPos
extractPosition (Parser.FuncDec{funcDecPos}) = funcDecPos
extractPosition (Parser.Function{functionPos}) = functionPos
extractPosition (Parser.DoBlock{doBlockPos}) = doBlockPos
extractPosition (Parser.ExternDec{externDecPos}) = externDecPos
extractPosition (Parser.Add{addPos}) = addPos
extractPosition (Parser.Sub{subPos}) = subPos
extractPosition (Parser.Mul{mulPos}) = mulPos
extractPosition (Parser.Div{divPos}) = divPos
extractPosition (Parser.Eq{eqPos}) = eqPos
extractPosition (Parser.Neq{neqPos}) = neqPos
extractPosition (Parser.Lt{ltPos}) = ltPos
extractPosition (Parser.Gt{gtPos}) = gtPos
extractPosition (Parser.Le{lePos}) = lePos
extractPosition (Parser.Ge{gePos}) = gePos
extractPosition (Parser.And{andPos}) = andPos
extractPosition (Parser.Or{orPos}) = orPos
extractPosition (Parser.Not{notPos}) = notPos
extractPosition (Parser.UnaryMinus{unaryMinusPos}) = unaryMinusPos
extractPosition (Parser.Placeholder{placeholderPos}) = placeholderPos
extractPosition (Parser.Discard{discardPos}) = discardPos
extractPosition (Parser.Import{importPos}) = importPos
extractPosition (Parser.Ref{refPos}) = refPos
extractPosition (Parser.Struct{structPos}) = structPos
extractPosition (Parser.StructAccess{structAccessPos}) = structAccessPos
extractPosition (Parser.ListLit{listLitPos}) = listLitPos
extractPosition (Parser.ListPattern{listPatternPos}) = listPatternPos
extractPosition (Parser.ListConcat{listConcatPos}) = listConcatPos
extractPosition (Parser.ListAdd{listAddPos}) = listAddPos
extractPosition (Parser.ArrayAccess{arrayAccessPos}) = arrayAccessPos
extractPosition (Parser.Modulo{moduloPos}) = moduloPos
extractPosition (Parser.Power{powerPos}) = powerPos
extractPosition (Parser.Target{targetPos}) = targetPos
extractPosition (Parser.Then{thenPos}) = thenPos
extractPosition (Parser.Pipeline{pipelinePos}) = pipelinePos
extractPosition (Parser.Lambda{lambdaPos}) = lambdaPos
extractPosition (Parser.Cast{castPos}) = castPos
extractPosition (Parser.TypeLit{typeLitPos}) = typeLitPos
extractPosition (Parser.Flexible{flexiblePos}) = flexiblePos
extractPosition (Parser.Trait{traitPos}) = traitPos
extractPosition (Parser.Impl{implPos = pos}) = pos
extractPosition (Parser.StrictEval{strictEvalPos}) = strictEvalPos
extractPosition (Parser.External{externalPos}) = externalPos
extractPosition (Parser.When{whenPos}) = whenPos
extractPosition (Parser.TupleLit{tupleLitPos}) = tupleLitPos
extractPosition (Parser.TupleAccess{tupleAccessPos}) = tupleAccessPos

getBinOpPosition :: Parser.Expr -> Parser.Expr -> AST.Position
getBinOpPosition x y =
    let px = extractPosition x
        py = extractPosition y
     in case (px, py) of
            (AST.Position (x1, x2), AST.Position (y1, y2))
                | x1 >= 0 && y1 >= 0 ->
                    AST.Position (min x1 y1, max x2 y2)
            (AST.Position (p1, _), _) | p1 >= 0 -> px
            (_, AST.Position (p2, _)) | p2 >= 0 -> py
            _ -> AST.anyPosition

compileBinOp :: CompileExpr -> String -> Parser.Expr -> Parser.Expr -> Parser.Type -> Instruction -> Compiler [Instruction]
compileBinOp compileExpr' opName x y expectedType op = do
    case (x, y) of
        (Parser.Flexible{}, _) -> doBinOp compileExpr' opName x y expectedType op
        (_, Parser.Flexible{}) -> doBinOp compileExpr' opName x y expectedType op
        _ -> do
            errorCountBefore <- getErrorCount
            let pos = getBinOpPosition x y
            _ <- compileExpr' (Parser.FuncCall{funcName = opName, funcArgs = [x, y], funcPos = pos}) expectedType
            errorCountAfter <- getErrorCount
            if errorCountAfter > errorCountBefore
                then return []
                else doBinOp compileExpr' opName x y expectedType op

doBinOp :: CompileExpr -> String -> Parser.Expr -> Parser.Expr -> Parser.Type -> Instruction -> Compiler [Instruction]
doBinOp compileExpr' opName x y expectedType op = do
    id' <- allocId
    functions' <- gets functions
    funcDecs' <- gets funcDecs
    let f = findAnyFunction (Data.Text.unpack $ Data.Text.toLower $ Data.Text.pack $ show op) functions'
    xType <- typeOf x
    yType <- typeOf y
    let argTypes = [xType, yType]
    let foundFunc = findFunction opName functions' argTypes
    let hasFuncDec = isJust $ find (\case Parser.FuncDec{name} -> name == opName; _ -> False) funcDecs'
    let aName = "__op_a_" ++ show id'
    let bName = "__op_b_" ++ show id'
    x' <- compileExpr' x expectedType
    y' <- compileExpr' y expectedType

    let hasFlexible = case (x, y) of
            (Parser.Flexible{}, _) -> True
            (_, Parser.Flexible{}) -> True
            _ -> False

    castInstrs <- case (x, y) of
        (Parser.Flexible{}, Parser.Flexible{}) -> internalError "Double cast"
        (Parser.Flexible{}, _) -> do
            let targetType = compileTypeFromType yType
            return ([LLoad aName] ++ targetType ++ [Cast, LLoad bName])
        (_, Parser.Flexible{}) -> do
            let targetType = compileTypeFromType xType
            return ([LLoad bName] ++ targetType ++ [Cast, LLoad aName, Swp])
        _ -> return [LLoad aName, LLoad bName]
    let finalOp = case (foundFunc, hasFuncDec, hasFlexible) of
            (Just fun, _, False) -> Call (funame fun)
            (Nothing, True, False) -> Call opName
            _ -> op
    case (x, y) of
        (Parser.Placeholder _, Parser.Placeholder _) -> return [PushPf (funame $ fromJust f) 0]
        (Parser.Placeholder _, _) -> return $ y' ++ [PushPf (funame $ fromJust f) 1]
        (_, Parser.Placeholder _) -> return $ x' ++ [PushPf (funame $ fromJust f) 1]
        _ -> return (x' ++ LStore aName : y' ++ [LStore bName] ++ castInstrs ++ [finalOp])
  where
    compileTypeFromType :: Parser.Type -> [Instruction]
    compileTypeFromType (Parser.StructT "Int" []) = [Push $ DInt 0]
    compileTypeFromType (Parser.StructT "Float" []) = [Push $ DFloat 0.0]
    compileTypeFromType (Parser.StructT "Double" []) = [Push $ DDouble 0.0]
    compileTypeFromType (Parser.StructT "Bool" []) = [Push $ DBool False]
    compileTypeFromType (Parser.StructT "Char" []) = [Push $ DChar '\0']
    compileTypeFromType (Parser.StructT "String" []) = [Push $ DString ""]
    compileTypeFromType (Parser.StructT "CPtr" []) = [Push $ DCPtr $ ptrToWordPtr nullPtr]
    compileTypeFromType _ = [Push DNone]

{- | Map from builtin name to the instruction(s) emitted after compiling
its two arguments in order.  Only covers the simple binary cases.
-}
unsafeBinaryBuiltins :: Data.Map.Map String [Instruction]
unsafeBinaryBuiltins =
    Data.Map.fromList
        [ ("unsafeAdd", [Add])
        , ("unsafeSub", [Sub])
        , ("unsafeMul", [Mul])
        , ("unsafeDiv", [Div])
        , ("unsafeMod", [Mod])
        , ("unsafePow", [Pow])
        , ("unsafeGt", [Gt])
        , ("unsafeLt", [Lt])
        , ("unsafeGe", [Lt, Not])
        , ("unsafeLe", [Gt, Not])
        , ("unsafeEq", [Eq])
        , ("unsafeNeq", [Eq, Not])
        , ("unsafeAnd", [And])
        , ("unsafeOr", [Or])
        , ("unsafeListIndex", [Index])
        , ("unsafeStructAccess", [AAccess])
        ]

compileUnsafeBuiltin :: CompileExpr -> String -> [Parser.Expr] -> Parser.Type -> Maybe (Compiler [Instruction])
compileUnsafeBuiltin compileExpr' funcName funcArgs expectedType =
    case Data.Map.lookup funcName unsafeBinaryBuiltins of
        Just instrs | [x, y] <- funcArgs -> Just $ bin x y instrs
        _ -> case (funcName, funcArgs) of
            ("unsafePrint", [x]) -> Just $ un x [Builtin Print]
            ("unsafeGetLine", []) -> Just $ return [Builtin GetLine]
            ("unsafeGetChar", []) -> Just $ return [Builtin GetChar]
            ("unsafeRandom", []) -> Just $ return [Builtin Random]
            ("unsafeListAdd", [y, x]) -> Just $ do
                x' <- compileExpr' x expectedType
                y' <- compileExpr' y expectedType
                return (x' ++ y' ++ [ListAdd 2])
            ("unsafeListLength", [x]) -> Just $ un x [Length]
            ("unsafeListSlice", [x, y, z]) -> Just $ tri x y z [Slice]
            ("unsafeKeys", [x]) -> Just $ un x [Keys]
            ("unsafeUpdate", [x, y, z]) -> Just $ tri x y z [Update]
            ("unsafeExit", [x]) -> Just $ un x [Exit]
            ("abs", [x]) -> Just $ un x [Abs]
            ("root", [x, Parser.FloatLit{floatValue = y}]) -> Just $ do
                x' <- compileExpr' x expectedType
                return (x' ++ [Push $ DFloat (1.0 / y), Pow])
            ("sqrt", [x]) -> Just $ do
                x' <- compileExpr' x expectedType
                return (x' ++ [Push $ DFloat 0.5, Pow])
            _ -> Nothing
  where
    bin x y instrs = do
        x' <- compileExpr' x expectedType
        y' <- compileExpr' y expectedType
        return (x' ++ y' ++ instrs)
    un x instrs = do
        x' <- compileExpr' x expectedType
        return (x' ++ instrs)
    tri x y z instrs = do
        x' <- compileExpr' x expectedType
        y' <- compileExpr' y expectedType
        z' <- compileExpr' z expectedType
        return (x' ++ y' ++ z' ++ instrs)

compileFuncCall :: CompileExpr -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
compileFuncCall compileExpr' (Parser.FuncCall{funcName, funcArgs, funcPos}) expectedType = do
    case compileUnsafeBuiltin compileExpr' funcName funcArgs expectedType of
        Just action -> action
        Nothing -> compileFuncCallMain compileExpr' funcName funcArgs funcPos expectedType
compileFuncCall _ _ _ = internalError "compileFuncCall: expected FuncCall"

compileFuncCallMain :: CompileExpr -> String -> [Parser.Expr] -> AST.Position -> Parser.Type -> Compiler [Instruction]
compileFuncCallMain compileExpr' funcName funcArgs funcPos expectedType = do
    let expectedTypeStructName = case expectedType of
            Parser.StructT name _ -> name
            _ -> ""
    implsForExpectedType <- implsFor expectedTypeStructName
    functions' <- gets functions
    funcDecs' <- gets funcDecs
    funcDefs' <- gets funcDefs
    curCon <- gets (\s -> case contextStack s of (c : _) -> c; [] -> "__outside")
    externals' <- gets externals
    contextPath' <- gets contextStack
    fbt <- gets functionsByTrait

    let maybeExpectedArgTypes = case find (\x -> x.name == funcName) funcDecs' of
            Just fd -> Just $ init $ Parser.types fd
            Nothing -> Nothing

    argTypes <-
        zipWithM
            ( \arg idx -> case (arg, maybeExpectedArgTypes) of
                (Parser.Lambda{lambdaArgs, lambdaBody}, Just expectedTypes) | idx < length expectedTypes -> do
                    let expectedArgType = expectedTypes !! idx
                    returnType <- typeOf lambdaBody
                    case expectedArgType of
                        Parser.Fn expectedArgTypes' expectedReturnType -> do
                            let argTypes' =
                                    if length lambdaArgs == length expectedArgTypes'
                                        then expectedArgTypes'
                                        else replicate (length lambdaArgs) Parser.Any
                            return $ Parser.Fn argTypes' returnType
                        _ -> do
                            returnType' <- typeOf lambdaBody
                            return $ Parser.Fn (replicate (length lambdaArgs) Parser.Any) returnType'
                _ -> typeOf arg
            )
            funcArgs
            [0 ..]
    let contexts = map (T.pack . intercalate "@") (inits (Data.List.Split.splitOn "@" curCon))
    let contextFunctions = firstJust (\context -> findFunction (Data.Text.unpack context ++ "@" ++ funcName) functions' argTypes) contexts
    let expectedTypeStr = typeToString expectedType
    let implsForExpectedTypePrefixes = map (\x -> x ++ "." ++ expectedTypeStr ++ "::" ++ funcName) implsForExpectedType

    traitMethodMatch <-
        if not (null funcArgs) && not (null argTypes)
            then do
                let firstArgType = head argTypes
                let firstArgTypeStr = typeToString firstArgType
                implsForFirstArg <- implsFor firstArgTypeStr
                let implsForFirstArgPrefixes = map (\x -> x ++ "." ++ firstArgTypeStr ++ "::" ++ funcName) implsForFirstArg
                let traitFunc = find (\(Function{baseName}) -> baseName `elem` implsForFirstArgPrefixes) functions'
                case traitFunc of
                    Just tf -> return $ Just tf
                    Nothing -> do
                        let traitMethod = find (\(for, n, _, newDec) -> for == firstArgTypeStr && n == funcName && length funcArgs <= length (Parser.types newDec) - 1) fbt
                        case traitMethod of
                            Just (_, _, fqn, newDec) -> do
                                let traitFuncFound = find (\(Function{baseName = bn}) -> bn == fqn) functions'
                                case traitFuncFound of
                                    Just tf -> return $ Just tf
                                    Nothing ->
                                        case findFunction funcName functions' argTypes of
                                            Just vf -> return $ Just vf
                                            Nothing -> return Nothing
                            Nothing -> return Nothing
            else return Nothing

    let virtualFunction = findFunction funcName functions' argTypes
    let virtualFunctionAny = findAnyFunction funcName functions'

    let fun = case traitMethodMatch of
            Just _ ->
                case virtualFunctionAny of
                    Just vf -> vf
                    Nothing ->
                        fromMaybe
                            (Function{baseName = funcName, funame = funcName ++ "#0", function = [], types = [], context = "__outside"})
                            virtualFunction
            Nothing -> case virtualFunction of
                Just tm -> tm
                Nothing -> case virtualFunctionAny of
                    Just tm -> tm
                    Nothing -> case find (\(Function{baseName}) -> baseName `elem` implsForExpectedTypePrefixes) functions' of
                        Just funf -> funf
                        Nothing -> case contextFunctions of
                            (Just lf) -> lf
                            Nothing ->
                                let isTraitMethod = any (\(_, n, _, _) -> n == funcName) fbt
                                    fallbackName = if isTraitMethod then funcName ++ "#0" else funcName
                                 in Function{baseName = unmangleFunctionName funcName, funame = fallbackName, function = [], types = [], context = "__outside"}

    let traitMethodDec = case traitMethodMatch of
            Just _ ->
                let firstArgType = head argTypes
                    firstArgTypeStr = typeToString firstArgType
                    traitMethod = find (\(for, n, _, newDec) -> for == firstArgTypeStr && n == funcName && length funcArgs <= length (Parser.types newDec) - 1) fbt
                 in case traitMethod of
                        Just (_, _, fqn, newDec) -> Just Parser.FuncDec{Parser.name = fqn, Parser.types = Parser.types newDec, Parser.generics = [], funcDecPos = anyPosition}
                        Nothing -> Nothing
            Nothing -> Nothing

    let expectedTypeTraitMethodDec =
            if not (null implsForExpectedType) && not (null argTypes)
                then
                    let expectedTypeMethod = find (\(for, n, _, newDec) -> for == expectedTypeStructName && n == funcName && length funcArgs <= length (Parser.types newDec) - 1) fbt
                     in case expectedTypeMethod of
                            Just (_, _, fqn, newDec) ->
                                let methodTypes = Parser.types newDec
                                    methodGenerics = Parser.generics newDec
                                 in if length methodTypes >= 2 && length argTypes == 1
                                        then Just Parser.FuncDec{Parser.name = fqn, Parser.types = methodTypes, Parser.generics = methodGenerics, funcDecPos = anyPosition}
                                        else Nothing
                            Nothing -> Nothing
                else Nothing
    let funcDec = case expectedTypeTraitMethodDec of
            Just fd -> Just fd
            Nothing -> case traitMethodDec of
                Just fd -> Just fd
                Nothing -> case find (\case Parser.FuncDec{name} -> name == baseName fun; _ -> False) funcDecs' of
                    Just fd -> case find (\(_, n, _, newDec) -> n == funcName && take (length funcArgs) (Parser.types newDec) == argTypes) fbt of
                        Just (_, _, fqn, newDec) -> Just Parser.FuncDec{Parser.name = fqn, Parser.types = Parser.types newDec, Parser.generics = Parser.generics fd, funcDecPos = anyPosition}
                        Nothing -> Just fd
                    Nothing -> Nothing
    let external = find (\x -> x.name == funcName) externals'

    let isLocal = T.pack (takeWhile (/= '@') curCon ++ "@") `isPrefixOf` T.pack fun.funame
    let callWay = if isLocal then CallLocal (funame fun) else Call (funame fun)

    (argsOk, msgs) <- case funcDec of
        Just fd -> do
            let expectedArgTypes = init $ Parser.types fd
            let generics = Parser.generics fd
            let isExpectedTypeTraitMethod = isJust expectedTypeTraitMethodDec
            let isTraitMethodForExpectedType = not (null implsForExpectedType) && any (\(for, n, _, _) -> for == expectedTypeStructName && n == funcName) fbt
            if null generics
                then do
                    if isTraitMethodForExpectedType && length argTypes == 1 && length expectedArgTypes == 1
                        then return (True, [])
                        else do
                            let zippedArgs = zip argTypes expectedArgTypes
                            wrongArgsBools <- mapM (uncurry typeCompatible) zippedArgs
                            let wrongArgs = [show t1 ++ " != " ++ show t2 | ((t1, t2), ok) <- zip zippedArgs wrongArgsBools, not ok]
                            let tooManyArgs = ["Too many arguments provided." | length argTypes > length expectedArgTypes]
                            return (null (wrongArgs ++ tooManyArgs), wrongArgs ++ tooManyArgs)
                else do
                    let tooManyArgs = ["Too many arguments provided." | length argTypes > length expectedArgTypes]
                    if (isExpectedTypeTraitMethod || isTraitMethodForExpectedType) && length argTypes == 1 && length expectedArgTypes == 1 && not (null generics)
                        then return (True, [])
                        else do
                            typesAcceptable <- functionTypesAcceptable argTypes expectedArgTypes generics
                            if typesAcceptable && null tooManyArgs
                                then return (True, [])
                                else do
                                    let wrongArgs = ["Generic type constraints not satisfied" | not typesAcceptable]
                                    return (False, wrongArgs ++ tooManyArgs)
        Nothing -> return (True, [])

    let (argsOkFinal, msgsFinal) = (argsOk, msgs)

    unless argsOkFinal $ cerror ("Function " ++ funcName ++ " called with incompatible types: " ++ intercalate ", " msgsFinal) funcPos

    let curConFuncDefs = reverse $ mapMaybe' (\ctx -> find (\case Parser.FuncDef{name} -> name == ctx; _ -> False) funcDefs') contextPath'
        curConFuncDefParamsNames = concat [[varName | Parser.Var{varName} <- args] | Parser.FuncDef _ args _ _ <- curConFuncDefs]
        inContext = funcName `elem` curConFuncDefParamsNames

    unless
        ( inContext
            || isJust external
            || isJust funcDec
        )
        $ cerror ("Function " ++ funcName ++ " not found.") funcPos

    case external of
        Just (External _ ereturnType _ from) -> do
            retT <- case typeToData ereturnType of
                DMap _ -> do
                    fields <- getStructFields from
                    return $ DMap $ Data.Map.fromList $ map (second typeToData) fields
                _ -> return $ typeToData ereturnType
            args' <- concatMapM (`compileExpr'` Parser.Any) (reverse funcArgs)
            return $ [Push retT] ++ args' ++ [CallFFI funcName from (length funcArgs)]
        Nothing ->
            case funcDec of
                (Just fd) -> do
                    let isExpectedTypeTraitMethod = isJust expectedTypeTraitMethodDec
                    if isExpectedTypeTraitMethod
                        then do
                            args' <- concatMapM (uncurry compileExpr') (zip funcArgs (tail fd.types))
                            let implFuncName = case expectedTypeTraitMethodDec of
                                    Just (Parser.FuncDec{name = n}) -> n
                                    Nothing -> internalError "Internal error: expectedTypeTraitMethodDec should be Just"
                            structDecs'' <- gets structDecs
                            let structDef = find (\case Parser.Struct{name = n} -> n == expectedTypeStructName; _ -> False) structDecs''
                            case structDef of
                                Just (Parser.Struct{fields = structFields}) -> do
                                    let valueField = case find (\(name, _) -> name == "value" || name == "inner") structFields of
                                            Just (fieldName, fieldType) -> Just (fieldName, fieldType)
                                            Nothing -> Nothing
                                    case valueField of
                                        Just (fieldName, _) -> do
                                            let structLit =
                                                    Parser.StructLit
                                                        { structLitName = expectedTypeStructName
                                                        , structLitFields = [(fieldName, head funcArgs)]
                                                        , structLitTypeArgs = case expectedType of
                                                            Parser.StructT _ typeArgs -> typeArgs
                                                            _ -> []
                                                        , structLitPos = funcPos
                                                        }
                                            compileExpr' structLit expectedType
                                        Nothing -> do
                                            let structLit =
                                                    Parser.StructLit
                                                        { structLitName = expectedTypeStructName
                                                        , structLitFields = []
                                                        , structLitTypeArgs = case expectedType of
                                                            Parser.StructT _ typeArgs -> typeArgs
                                                            _ -> []
                                                        , structLitPos = funcPos
                                                        }
                                            compileExpr' structLit expectedType
                                Nothing -> do
                                    let implFunc = find (\(Function{baseName = bn}) -> bn == implFuncName) functions'
                                    let implFuname = case implFunc of
                                            Just (Function{funame = fn}) -> fn
                                            Nothing -> implFuncName
                                    let implIsLocal = case implFunc of
                                            Just (Function{funame = fn}) -> T.pack (takeWhile (/= '@') curCon ++ "@") `isPrefixOf` T.pack fn
                                            Nothing -> False
                                    let implCallWay = if implIsLocal then CallLocal implFuname else Call implFuname
                                    let dummySelf = [Push $ DString expectedTypeStructName, Push $ DString "__name", Push $ DList [], Push $ DString "__traits", PackMap 4]
                                    return $ dummySelf ++ args' ++ [implCallWay]
                        else
                            if length funcArgs == length (Parser.types fd) - 1
                                then concatMapM (uncurry compileExpr') (zip funcArgs fd.types) >>= \args' -> return (args' ++ [callWay])
                                else
                                    concatMapM (uncurry compileExpr') (zip funcArgs fd.types) >>= \args' ->
                                        return $
                                            args'
                                                ++ [PushPf (funame fun) (length args')]
                Nothing -> do
                    concatMapM (\arg -> typeOf arg >>= compileExpr' arg) funcArgs >>= \args' ->
                        return $
                            args'
                                ++ [ LLoad funcName
                                   , CallS
                                   ]
  where
    -- Local mapMaybe to avoid import collision
    mapMaybe' :: (a -> Maybe b) -> [a] -> [b]
    mapMaybe' _ [] = []
    mapMaybe' f (x : xs) = case f x of
        Just y -> y : mapMaybe' f xs
        Nothing -> mapMaybe' f xs

compileFuncDec :: CompileExpr -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
compileFuncDec _compileExpr' fd@(Parser.FuncDec{name, types, generics, funcDecPos}) _ = do
    let funcGenericNames = map (\(Parser.GenericExpr n _) -> n) generics
    forM_ types $ \type' -> do
        validateTypeParameters funcGenericNames type' funcDecPos ("function declaration " ++ name)
    modify (\s -> s{funcDecs = fd : funcDecs s})
    return [] -- Function declarations are only used for compilation
  where
    validateTypeParameters :: [String] -> Parser.Type -> AST.Position -> String -> Compiler ()
    validateTypeParameters validGenericNames typeArg pos context = do
        structDecs' <- gets structDecs
        let knownStructNames = [n | Parser.Struct{name = n} <- structDecs']
        let knownTraitNames = [] :: [String] -- traits are validated elsewhere
        checkType validGenericNames typeArg pos context knownStructNames knownTraitNames

    checkType :: [String] -> Parser.Type -> AST.Position -> String -> [String] -> [String] -> Compiler ()
    checkType validGenericNames (Parser.StructT typeName typeArgs) pos context knownStructNames _knownTraitNames = do
        unless
            ( typeName `elem` validGenericNames
                || typeName `elem` knownStructNames
                || typeName `elem` ["Int", "Float", "Double", "Bool", "String", "Char", "CPtr", "Unit", "Self"]
            )
            $ cerror ("Unknown type '" ++ typeName ++ "' in " ++ context) pos
        mapM_ (\ta -> checkType validGenericNames ta pos context knownStructNames _knownTraitNames) typeArgs
    checkType validGenericNames (Parser.List t) pos context ks kt =
        checkType validGenericNames t pos context ks kt
    checkType validGenericNames (Parser.Tuple ts) pos context ks kt =
        mapM_ (\t -> checkType validGenericNames t pos context ks kt) ts
    checkType validGenericNames (Parser.Fn args ret) pos context ks kt = do
        mapM_ (\t -> checkType validGenericNames t pos context ks kt) args
        checkType validGenericNames ret pos context ks kt
    checkType _ _ _ _ _ _ = return ()
compileFuncDec _ _ _ = internalError "compileFuncDec: expected FuncDec"

compileFuncDef :: CompileExpr -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
compileFuncDef compileExpr' fd@(Parser.FuncDef{name = origName, args, body}) expectedType = do
    curCon <- gets (\s -> case contextStack s of (c : _) -> c; [] -> "__outside")
    funs <- gets functions
    let previousContext = curCon
    let previousContextPath = if curCon == "__outside" then ["__outside"] else Data.List.Split.splitOn "@" curCon
    let name =
            if "." `isInfixOf` origName || curCon == "__outside"
                then origName
                else curCon ++ "@" ++ origName
    let origName' = name
    let isFirst = isNothing $ find (\x -> x.baseName == origName') funs
    let newContextPath = if curCon == "__outside" then [origName] else previousContextPath ++ [origName]
    modify (\s -> s{contextStack = newContextPath})
    funcDecs' <- gets funcDecs
    when (isNothing $ find (\case Parser.FuncDec{name = name'} -> name' == name; _ -> False) funcDecs') $ modify (\s -> s{funcDecs = Parser.FuncDec name (replicate (length args + 1) expectedType) [] anyPosition : funcDecs s})

    funame' <- if name /= "main" then ((name ++ "#") ++) . show <$> allocId else return "main"
    modify (\s -> s{functions = Function name funame' [] [] curCon : functions s})
    modify (\s -> s{funcDefs = fd : funcDefs s})
    funcDecs'' <- gets funcDecs

    let funcDec = fromJust $ find (\case Parser.FuncDec{name = name'} -> name' == name; _ -> False) funcDecs''

    body' <- compileExpr' body (last $ Parser.types funcDec)

    let expectedReturnType = last $ Parser.types funcDec
    let argTypes = init $ Parser.types funcDec
    let argsWithTypes = zip args argTypes
    let paramBindings = concatMap extractParamBindings argsWithTypes
          where
            extractParamBindings (Parser.Var{varName}, t) = [Let{name = varName, vtype = t, context = name}]
            extractParamBindings _ = []
    letsBefore <- gets lets
    modify (\s -> s{lets = paramBindings ++ lets s})
    bodyType <- safeTypeOf body
    modify (\s -> s{lets = letsBefore})
    let generics = Parser.generics funcDec
    let genericNames = map (\(Parser.GenericExpr n _) -> n) generics
    let genericReturnInfo = case expectedReturnType of
            Parser.StructT t []
                | t `elem` genericNames ->
                    find (\(Parser.GenericExpr n _) -> n == t) generics
            _ -> Nothing
    let shouldCheck =
            expectedReturnType /= Parser.Any
                && expectedReturnType /= Parser.Unknown
                && bodyType /= Parser.Unknown
                && bodyType /= Parser.Any
    when shouldCheck $ case genericReturnInfo of
        Just (Parser.GenericExpr genericName (Just constraint)) -> do
            let constraintName = typeToString constraint
            let bodyTypeName = typeToString bodyType
            let isGenericItself = bodyTypeName == genericName
            bodyImpls <- implsFor bodyTypeName
            let satisfiesConstraint = isGenericItself || constraintName `elem` bodyImpls || bodyTypeName == constraintName
            unless satisfiesConstraint $
                cerror ("Return type mismatch in function `" ++ origName ++ "`: " ++ show bodyType ++ " does not implement " ++ constraintName) (extractPosition body)
        Just (Parser.GenericExpr _ Nothing) -> return ()
        Nothing -> do
            compatible <- typeCompatible bodyType expectedReturnType
            unless compatible $
                cerror ("Return type mismatch in function `" ++ origName ++ "`: expected " ++ show expectedReturnType ++ ", got " ++ show bodyType) (extractPosition body)

    nextFunId <- gets lastLabel
    args' <- concatMapM (`compileParameter` (name, funame', nextFunId)) (reverse (filter (\case Parser.Placeholder _ -> False; _ -> True) args))
    let function = Label funame' : [StoreSideStack | isFirst] ++ [LoadSideStack | not isFirst] ++ args' ++ body' ++ [ClearSideStack] ++ [Ret | name /= "main"]
    modify (\s -> s{functions = Function name funame' function funcDec.types curCon : functions s})
    let restoredContextPath = if previousContext == "__outside" then ["__outside"] else Data.List.Split.splitOn "@" previousContext
    modify (\s -> s{contextStack = restoredContextPath})
    return [] -- Function definitions get appended at the last stage of compilation
  where
    compileParameter :: Parser.Expr -> (String, String, Int) -> Compiler [Instruction]
    compileParameter pat (funcName, _currentFuname, nextFunId) = do
        let nextFunName = funcName ++ "#" ++ show nextFunId
        compilePatternMatch compileExpr' pat nextFunName expectedType
compileFuncDef _ _ _ = internalError "compileFuncDef: expected FuncDef"

compileFunction :: CompileExpr -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
compileFunction compileExpr' (Parser.Function{def = [Parser.FuncDef{body = Parser.StrictEval{strictEvalExpr = e}}], dec}) expectedType = do
    let name = dec.name
    evaledExpr <- compileExpr' e expectedType
    return $ evaledExpr ++ [LStore name]
compileFunction compileExpr' (Parser.Function{def = a, dec = b@Parser.FuncDec{name = _name, types = _types}}) _ = do
    curCon <- gets (\s -> case contextStack s of (c : _) -> c; [] -> "__outside")
    let mangledName =
            if "." `isInfixOf` b.name || curCon == "__outside"
                then b.name
                else curCon ++ "@" ++ b.name
    let mangledDec = b{Parser.name = mangledName}
    compileExpr' mangledDec (last b.types) >> mapM_ (`compileExpr'` last b.types) a >> return []
compileFunction _ _ _ = internalError "compileFunction: expected Function"

compileLambda :: CompileExpr -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
compileLambda compileExpr' (Parser.Lambda{lambdaArgs, lambdaBody}) expectedType = do
    fId <- allocId
    curCon <- gets (\s -> case contextStack s of (c : _) -> c; [] -> "__outside")
    let args' = if lambdaArgs == [Parser.Placeholder anyPosition] then [] else lambdaArgs
    let name = "__lambda" ++ show fId
    let def = Parser.FuncDef{name = name, args = args', body = lambdaBody, funcDefPos = anyPosition}
    let dec = Parser.FuncDec{name = name, types = replicate (length args' + 1) Parser.Any, generics = [], funcDecPos = anyPosition}
    let fun = Parser.Function{def = [def], dec = dec, functionPos = anyPosition}
    _ <- compileExpr' fun expectedType
    lets' <- gets functions
    let fullName = (fromJust $ findAnyFunction (curCon ++ "@" ++ name) lets').funame
    return [PushPf fullName 0]
compileLambda _ _ _ = internalError "compileLambda: expected Lambda"

compileVar :: CompileExpr -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
compileVar compileExpr' (Parser.Var{varName, varPos}) expectedType = do
    functions' <- gets functions
    curCon <- gets (\s -> case contextStack s of (c : _) -> c; [] -> "__outside")
    externals' <- gets externals
    let fun =
            any ((== varName) . baseName) functions'
                || any ((\context -> any ((== context ++ "@" ++ varName) . baseName) functions') . intercalate "@") (inits (Data.List.Split.splitOn "@" curCon))
                || any ((== varName) . (Data.Text.unpack . last . splitOn "::") . fromString . baseName) functions'
    if fun || varName `elem` internalFunctions || varName `elem` map (\f -> f.name) externals'
        then (`compileExpr'` expectedType) (Parser.FuncCall{funcName = varName, funcArgs = [], funcPos = varPos})
        else return [LLoad varName]
compileVar _ _ _ = internalError "compileVar: expected Var"

compileLet :: CompileExpr -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
compileLet compileExpr' (Parser.Let{letName, letValue}) _ = do
    curCon <- gets (\s -> case contextStack s of (c : _) -> c; [] -> "__outside")
    case letValue of
        Parser.Function{def, dec} -> do
            let renamedDec = case dec of
                    Parser.FuncDec{} -> dec{Parser.name = letName}
                    _ -> dec
                renamedDef =
                    map
                        ( \d -> case d of
                            Parser.FuncDef{args, body, funcDefPos} -> Parser.FuncDef{name = letName, args = args, body = body, funcDefPos = funcDefPos}
                            _ -> d
                        )
                        def
            let fun = Parser.Function{def = renamedDef, dec = renamedDec, functionPos = Parser.anyPosition}
            compileExpr' fun Parser.Any
        _ -> do
            typeOf letValue >>= \v -> modify (\s -> s{lets = Let{name = letName, vtype = v, context = curCon} : lets s})
            value' <- typeOf letValue >>= compileExpr' letValue
            return $ value' ++ [LStore letName]
compileLet _ _ _ = internalError "compileLet: expected Let"

compileParenApply :: CompileExpr -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
compileParenApply compileExpr' (Parser.ParenApply{parenApplyExpr, parenApplyArgs}) expectedType = do
    case parenApplyExpr of
        Parser.StructAccess{structAccessStruct = Parser.Var{varName = moduleName}, structAccessField = Parser.Var{varName = funcName}} -> do
            let qualifiedName = moduleName ++ "." ++ funcName
            compileExpr' (Parser.FuncCall{funcName = qualifiedName, funcArgs = parenApplyArgs, funcPos = extractPosition parenApplyExpr}) expectedType
        _ -> do
            fun <- compileExpr' parenApplyExpr expectedType
            args <- concatMapM (`compileExpr'` expectedType) parenApplyArgs
            return $ args ++ fun ++ [CallS]
compileParenApply _ _ _ = internalError "compileParenApply: expected ParenApply"

compileExternal :: CompileExpr -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
compileExternal _ (Parser.External{externalArgs = []}) _ = return []
compileExternal compileExpr' (Parser.External{externalName = from, externalArgs = (Parser.FuncDec{name, types} : xs)}) expectedType = do
    modify (\s -> s{externals = External{name, returnType = last types, args = init types, from} : externals s})
    _ <- compileExpr' (Parser.External{externalName = from, externalArgs = xs, externalPos = anyPosition}) expectedType
    return []
compileExternal _ _ _ = internalError "compileExternal: expected External"
