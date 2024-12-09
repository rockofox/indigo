{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Verifier where

import AST
import BytecodeCompiler (preludeFile)
import BytecodeCompiler qualified
import Control.Monad
import Control.Monad.Loops (allM)
import Control.Monad.State.Lazy hiding (state)
import Data.Functor ((<&>))
import Data.List (find)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Void
import Parser
import Text.Megaparsec hiding (State)
import Util
import VM qualified

data VerifierState = VerifierState {frames :: [VerifierFrame], topLevel :: Bool, structDecs :: [Expr]} deriving (Show)

data VerifierFrame = VerifierFrame
    { bindings :: Set.Set VBinding
    , ttypes :: Map.Map String VType
    , ftype :: AST.Type
    , fname :: String
    }
    deriving (Show)

data VType = VType
    { implements :: [String]
    }
    deriving (Show, Eq, Ord)

data VBinding = VBinding
    { name :: String
    , args :: [AST.Type]
    , ttype :: AST.Type
    , generics :: [AST.GenericExpr]
    }
    deriving (Show, Eq, Ord)

findMatchingBinding :: String -> StateT VerifierState IO (Maybe VBinding)
findMatchingBinding name = do
    frames' <- frames <$> get
    let matchingBindings = concatMap (Set.toList . Set.filter (\binding -> Verifier.name binding == name) . bindings) frames'
    if null matchingBindings
        then return Nothing
        else return $ Just $ head matchingBindings

findMatchingBindings :: String -> StateT VerifierState IO [VBinding]
findMatchingBindings name = do
    frames' <- frames <$> get
    let matchingBindings = concatMap (Set.toList . Set.filter (\binding -> Verifier.name binding == name) . bindings) frames'
    return matchingBindings

listPatternToBindings :: Expr -> Type -> [VBinding]
listPatternToBindings (ListPattern exprs) (List t) = do
    let singulars = init exprs
    let Var restName _ = last exprs
    concatMap (\case (Var name _) -> [VBinding{name = name, args = [], ttype = t, generics = []}]; _ -> []) singulars ++ [VBinding{name = restName, args = [], ttype = List t, generics = []}]
listPatternToBindings (ListPattern exprs) (StructT "String") = do
    let singulars = init exprs
    let Var restName _ = last exprs
    concatMap (\case (Var name _) -> [VBinding{name = name, args = [], ttype = StructT "Char", generics = []}]; _ -> []) singulars ++ [VBinding{name = restName, args = [], ttype = StructT "String", generics = []}]
listPatternToBindings x y = error $ "listPatternToBinding called with non-list-pattern: " ++ show x ++ " " ++ show y

typeOf' :: Expr -> StateT VerifierState IO Type
typeOf' (Var name _) = do
    typeOf' (FuncCall name [] zeroPosition)
typeOf' (FuncCall name args _) = do
    matchingBinding <- findMatchingBinding name
    if isJust matchingBinding
        then do
            let actualArgNum = length args
            let formalArgNum = length (fromJust matchingBinding).args
            if actualArgNum == formalArgNum
                then return $ maybe Unknown ttype matchingBinding
                else return Fn{args = take actualArgNum (fromJust matchingBinding).args, ret = (fromJust matchingBinding).args !! actualArgNum}
        else return Unknown
typeOf' (Add a _) = typeOf' a
typeOf' (Sub a _) = typeOf' a
typeOf' (Mul a _) = typeOf' a
typeOf' (Div a _) = typeOf' a
typeOf' (Power a _) = typeOf' a
typeOf' (UnaryMinus x) = typeOf' x
typeOf' (If _ b _) = typeOf' b
typeOf' (Modulo x _) = typeOf' x
typeOf' (ListConcat x _) = typeOf' x
typeOf' (StructAccess n@(Var{}) (Var fieldName _)) = do
    (StructT s) <- typeOf' n
    -- (Struct _ fields _ _ _) <- gets structDecs <&> fromJust . find (\case Struct{name = name'} -> name' == s; _ -> False)
    structDecs' <- gets structDecs
    case find (\case Struct{name = name'} -> name' == s; _ -> False) structDecs' of
        Just (Struct _ fields _ _ _) ->
            return $ fromMaybe Unknown (lookup fieldName fields)
        Nothing -> return Unknown
        _ -> error "Impossible"
typeOf' (Pipeline _ b) = typeOf' b
typeOf' (Flexible _) = return Unknown
typeOf' (Then _ b) = typeOf' b
typeOf' (StrictEval x) = typeOf' x
typeOf' x@ListPattern{} = return $ typeOf x
typeOf' x@BoolLit{} = return $ typeOf x
typeOf' x@IntLit{} = return $ typeOf x
typeOf' x@FloatLit{} = return $ typeOf x
typeOf' x@StringLit{} = return $ typeOf x
typeOf' x@StructLit{} = return $ typeOf x
typeOf' x@DoubleLit{} = return $ typeOf x
typeOf' x@CharLit{} = return $ typeOf x
typeOf' x@Discard{} = return $ typeOf x
typeOf' x@ListLit{} = return $ typeOf x
typeOf' x@ArrayAccess{} = return $ typeOf x
typeOf' x@Not{} = return $ typeOf x
typeOf' x@Eq{} = return $ typeOf x
typeOf' x@Neq{} = return $ typeOf x
typeOf' x@Lt{} = return $ typeOf x
typeOf' x@Gt{} = return $ typeOf x
typeOf' x@Le{} = return $ typeOf x
typeOf' x@Ge{} = return $ typeOf x
typeOf' x@And{} = return $ typeOf x
typeOf' x@Or{} = return $ typeOf x
typeOf' x@DoBlock{} = return $ typeOf x
typeOf' x@FuncDef{} = return $ typeOf x
typeOf' x@FuncDec{} = return $ typeOf x
typeOf' x@Function{} = return $ typeOf x
typeOf' x@ExternDec{} = return $ typeOf x
typeOf' x@Lambda{} = return $ typeOf x
typeOf' x@Cast{} = return $ typeOf x
typeOf' x@TypeLit{} = return $ typeOf x
typeOf' x@Target{} = return $ typeOf x
typeOf' x@Ref{} = return $ typeOf x
typeOf' x@Import{} = return $ typeOf x
typeOf' x@Struct{} = return $ typeOf x
typeOf' x@Trait{} = return $ typeOf x
typeOf' x@Impl{} = return $ typeOf x
typeOf' x@External{} = return $ typeOf x
typeOf' x@Placeholder{} = return $ typeOf x
typeOf' x@Let{} = return $ typeOf x
typeOf' x@ParenApply{} = return $ typeOf x
typeOf' x@ListAdd{} = return $ typeOf x
typeOf' x = error $ "typeOf' called with " ++ show x

compareTypes' :: Type -> Type -> [AST.GenericExpr] -> StateT VerifierState IO Bool
compareTypes' (List x) (List y) generics = compareTypes' x y generics
compareTypes' (List Any) (StructT "String") _ = return True
compareTypes' (List (StructT "Char")) (StructT "String") _ = return True
compareTypes' (StructT "String") (List (StructT "Char")) _ = return True
compareTypes' (StructT "String") (List Any) _ = return True
compareTypes' aT (StructT b) generics = case aT of
    StructT a -> do
        let gen = find (\(GenericExpr name _) -> name == b) generics
        case gen of
            Just (GenericExpr _ (Just (StructT t))) -> compStructs a t
            Just (GenericExpr _ _) -> return True
            Nothing -> compStructs a b
    Unknown -> return True
    _ -> do
        let gen = find (\(GenericExpr name _) -> name == b) generics
        case gen of
            Just (GenericExpr _ (Just t)) -> return $ compareTypes aT t
            Just (GenericExpr _ Nothing) -> return True
            Nothing -> return False
  where
    compStructs :: String -> String -> StateT VerifierState IO Bool
    compStructs structA structB = do
        rootFrame <- gets frames <&> last
        let ttypes' = ttypes rootFrame
        let a' = Map.findWithDefault (VType{implements = []}) structA ttypes'
        let _b' = Map.findWithDefault (VType{implements = []}) structB ttypes'
        return $ structB `elem` implements a' || structA == structB
compareTypes' a b _ = return $ compareTypes a b

allTheSame :: [Type] -> Bool
allTheSame xs = all (compareSame $ head xs) (tail xs)
  where
    compareSame :: Type -> Type -> Bool
    compareSame (List x) (List y) = compareSame x y
    compareSame x y = x == y

functionTypesAcceptable :: [Type] -> [Type] -> [AST.GenericExpr] -> StateT VerifierState IO Bool
functionTypesAcceptable use def generics = do
    let genericNames = map (\(GenericExpr name _) -> name) generics
    let useAndDef = zip use def
    let udStructs = concatMap (\(a, b) -> case (a, b) of (_, StructT _) -> [(a, b)]; (List c'@(StructT _), List c@(StructT _)) -> [(c', c)]; _ -> []) useAndDef
    let udStructsGeneric = filter (\case (_, StructT b) -> b `elem` genericNames; (_, List (StructT b)) -> b `elem` genericNames; _ -> error "Impossible") udStructs
    let groupedUdStructsGeneric = map (\x -> (x, fst <$> filter (\case (_, StructT b) -> b == x; (_, List (StructT b)) -> b == x; _ -> error "Impossible") udStructsGeneric)) genericNames
    let genericsMatch = all (\(_, types) -> allTheSame types) groupedUdStructsGeneric
    typesMatch' <- allM (uncurry $ uncurry compareTypes') $ zip (zip use def) [generics]
    return $ typesMatch' && genericsMatch

initVerifierState :: VerifierState
initVerifierState =
    VerifierState
        { frames = [VerifierFrame{bindings = Set.fromList [], ttypes = Map.empty, ftype = Any, fname = "__outside"}] -- TODO: actually import prelude
        , topLevel = True
        , structDecs = []
        }

currentFrame :: StateT VerifierState IO VerifierFrame
currentFrame = head . frames <$> get

runRefinement :: Expr -> Expr -> IO Bool
runRefinement refinement value = do
    let prog =
            Program
                [ FuncDec "__refinement" [Any, StructT "Bool"] []
                , FuncDef "__refinement" [Var "it" zeroPosition] refinement
                , FuncDec "main" [StructT "IO"] []
                , FuncDef
                    "main"
                    []
                    ( DoBlock
                        [ FuncCall "__refinement" [value] zeroPosition
                        ]
                    )
                ]
    compiled <- evalStateT (BytecodeCompiler.compileProgram prog) (BytecodeCompiler.initCompilerState prog)
    let mainPc = BytecodeCompiler.locateLabel compiled "main"
    result <- VM.runVMVM $ (VM.initVM (V.fromList compiled)){VM.pc = mainPc, VM.callStack = [VM.StackFrame{returnAddress = mainPc, locals = []}]}
    return $ case VM.stack result of
        [VM.DBool b] -> b
        _ -> error $ "Refinement did not return a boolean, but " ++ show (VM.stack result) ++ " instead"

verifyProgram :: String -> Text -> [Expr] -> IO (Either (ParseErrorBundle Text Void) ())
verifyProgram name input exprs = evalStateT (verifyProgram' name input exprs) initVerifierState

verifyProgram' :: String -> Text -> [Expr] -> StateT VerifierState IO (Either (ParseErrorBundle Text Void) ())
verifyProgram' name source exprs = do
    let initialState =
            PosState
                { pstateInput = source
                , pstateOffset = 0
                , pstateSourcePos = initialPos name
                , pstateTabWidth = defaultTabWidth
                , pstateLinePrefix = ""
                }
    when (name /= "__prelude") $ do
        prelude <- liftIO preludeFile
        let parsedPrelude = parseProgram (T.pack prelude) initCompilerFlags{needsMain = False}
        case parsedPrelude of
            Left err -> error $ "Parse error: " ++ errorBundlePretty err
            Right (Program exprs') -> do
                _ <- concatMapM verifyExpr exprs'
                return ()
    errors <- concatMapM verifyExpr exprs
    if null errors
        then return $ Right ()
        else return $ Left $ ParseErrorBundle{bundleErrors = NonEmpty.fromList errors, bundlePosState = initialState}

verifyMultiple :: [Expr] -> StateT VerifierState IO [ParseError s e]
verifyMultiple = concatMapM verifyExpr

structLitToBindings :: Expr -> Type -> [VBinding]
structLitToBindings (StructLit _ fields _) _ = map (\case (_, Var name _) -> VBinding{name = name, ttype = Any, args = [], generics = []}; _ -> error "Impossible") fields
structLitToBindings x y = error $ "structLitToBindings called with " ++ show x ++ " and " ++ show y

verifyExpr :: Expr -> StateT VerifierState IO [ParseError s e]
verifyExpr (FuncDef name args body) = do
    -- TODO: Position
    b <- findMatchingBinding name
    case b of
        Just binding -> do
            currentFrame' <- currentFrame
            modify (\state -> state{frames = currentFrame'{bindings = Set.map (\b' -> if Verifier.name b' == name && b'.ttype == Any then b'{ttype = typeOf body} else b') (bindings (head (frames state)))} : tail (frames state)})
            let argsAndTypes = zip args binding.args
            let argsAsBindings = concatMap argToBindings argsAndTypes
            let bType = if binding.ttype == Any then typeOf body else binding.ttype
            modify (\state -> state{frames = (VerifierFrame{bindings = Set.fromList argsAsBindings, ttypes = Map.empty, ftype = bType, fname = name}) : frames state})
            types <- mapM typeOf' args
            modify (\state -> state{frames = (VerifierFrame{bindings = Set.insert (VBinding{name = name, args = types, ttype = bType, generics = []}) (bindings (head (frames state))), ttypes = Map.empty, ftype = bType, fname = name}) : tail (frames state)})
            modify (\state -> state{topLevel = True})
            bodyErrors <- verifyExpr body
            modify (\state -> state{frames = tail (frames state)})
            return bodyErrors
          where
            argToBindings :: (Expr, Type) -> [VBinding]
            argToBindings (Var name' _, Fn args' ret) = [VBinding{name = name', args = args', ttype = ret, generics = []}]
            argToBindings (Var name' _, ttype') = [VBinding{name = name', args = [], ttype = ttype', generics = []}]
            argToBindings (l@ListPattern{}, t) = listPatternToBindings l t
            argToBindings (s@StructLit{}, t) = structLitToBindings s t
            argToBindings _ = []
        Nothing -> return [FancyError 0 (Set.singleton (ErrorFail $ "Function " ++ name ++ " is missing a declaration"))]
verifyExpr (FuncDec name dtypes generics) = do
    currentFrame' <- currentFrame
    let types = init dtypes ++ typeErase [last dtypes]
    let (arguments, returnType) = if null types then ([], Any) else (init types, last types)
    modify (\state -> state{frames = currentFrame'{bindings = Set.insert (VBinding{name = name, args = arguments, ttype = returnType, generics}) (bindings (head (frames state)))} : tail (frames state)})
    return []
  where
    typeErase :: [Type] -> [Type]
    typeErase = map typeErase'
      where
        typeErase' :: Type -> Type
        typeErase' (StructT structName) =
            case find (\(GenericExpr name' _) -> name' == structName) generics of
                Just (GenericExpr _ (Just t)) -> t
                Just (GenericExpr _ Nothing) -> Any
                _ -> StructT structName
        typeErase' t = t
verifyExpr (DoBlock exprs) = concatMapM verifyExpr' exprs
  where
    verifyExpr' :: Expr -> StateT VerifierState IO [ParseError s e]
    verifyExpr' e = do
        modify (\state -> state{topLevel = True})
        verifyExpr e
verifyExpr (Function def dec) = do
    b <- verifyExpr dec
    a <- verifyMultiple def
    return $ a ++ b
verifyExpr (Trait _ methods) = do
    currentFrame' <- currentFrame
    modify (\state -> state{frames = currentFrame'{bindings = Set.union (bindings (head (frames state))) (Set.fromList (map (\(FuncDec name' types _) -> VBinding{name = name', args = types, ttype = Any, generics = []}) methods))} : tail (frames state)})
    return []
verifyExpr (Let name expr) = do
    currentFrame' <- currentFrame
    letType <- typeOf' expr
    modify (\state -> state{frames = currentFrame'{bindings = Set.insert (VBinding{name = name, args = [], ttype = letType, generics = []}) (bindings (head (frames state)))} : tail (frames state)})
    verifyExpr expr
verifyExpr (Var name (Position (start, _))) = do
    matchingBinding <- findMatchingBinding name
    return [FancyError start (Set.singleton (ErrorFail $ "Could not find relevant binding for " ++ name)) | isNothing matchingBinding]
verifyExpr (StructAccess _ _) = return [] -- TODO
verifyExpr s@(Struct{name = name, is = is}) = do
    rootFrame <- head . frames <$> get
    modify (\state -> state{frames = rootFrame{ttypes = Map.alter (\case Just (VType{implements = impls}) -> Just (VType{implements = is ++ impls}); Nothing -> Just (VType{implements = is})) name (ttypes rootFrame)} : tail (frames state)})
    modify (\state -> state{structDecs = s : structDecs state}) >> return []
verifyExpr sl@(StructLit structName _ (Position (start, _))) = do
    struct <- gets structDecs <&> find (\case Struct{name = name'} -> name' == structName; _ -> False)
    case struct of
        Nothing -> return [FancyError 0 (Set.singleton (ErrorFail $ "Could not find relevant struct for " ++ structName))]
        Just st -> do
            case st.refinement of
                Just rf -> do
                    r <- liftIO $ runRefinement rf sl
                    (if r then return [] else return [FancyError start (Set.singleton (ErrorFail $ "Refinement failed (" ++ st.refinementSrc ++ ")"))])
                Nothing -> return []
verifyExpr (Impl trait for _) = do
    rootFrame <- head . frames <$> get
    modify (\state -> state{frames = rootFrame{ttypes = Map.alter (\case Just (VType{implements = impls}) -> Just (VType{implements = trait : impls}); Nothing -> Just (VType{implements = [trait]})) for (ttypes rootFrame)} : tail (frames state)})
    return []
verifyExpr (FuncCall name args (Position (start, _))) = do
    currentFrame' <- currentFrame
    topLevel' <- gets topLevel
    matchingBindings <- findMatchingBindings name
    modify (\s -> s{topLevel = False})
    eArgs <- concatMapM verifyExpr args
    argumentTypes <- mapM typeOf' args
    fta <- case matchingBindings of
        [] -> return False
        bindings -> do
            let fta' = mapM (\binding -> functionTypesAcceptable argumentTypes binding.args binding.generics) bindings
            or <$> fta'
    eNoMatchi <- case matchingBindings of
        [] -> return []
        bindings -> do
            concatMapM
                ( \binding -> do
                    matchi <- compareTypes' (ftype currentFrame') (ttype binding) binding.generics
                    return [FancyError start (Set.singleton (ErrorFail ("Type `" ++ show binding.ttype ++ "` of `" ++ binding.name ++ "` is incompatible with type `" ++ show currentFrame'.ftype ++ "` of " ++ currentFrame'.fname))) | topLevel' && currentFrame'.fname /= "__lambda" && not matchi]
                )
                bindings
    let eTypes = concatMap (\matchingBinding -> ([FancyError start (Set.singleton (ErrorFail ("Argument types do not match on " ++ name ++ ", expected: " ++ show matchingBinding.args ++ ", got: " ++ show argumentTypes))) | not fta])) matchingBindings
    return $ [FancyError start (Set.singleton (ErrorFail $ "Could not find relevant binding for " ++ name)) | null matchingBindings] ++ eArgs ++ eTypes ++ eNoMatchi
verifyExpr (Lambda args body) = do
    let argsAsBindings = map (\(Var name' _) -> VBinding{name = name', args = [], ttype = Any, generics = []}) args
    modify (\state -> state{frames = (VerifierFrame{bindings = Set.fromList argsAsBindings, ttypes = Map.empty, ftype = Any, fname = "__lambda"}) : frames state})
    bodyErrors <- verifyExpr body
    modify (\state -> state{frames = tail (frames state)})
    return bodyErrors
verifyExpr (Parser.Import{objects = o, from = from, as = as, qualified = qualified}) = do
    when (o /= ["*"]) $ error "Only * imports are supported right now"
    let convertedPath = map (\x -> if x == '@' then '/' else x) from
    i <- liftIO $ readFile $ convertedPath ++ ".in"
    let expr = case parseProgram (Data.Text.pack i) CompilerFlags{verboseMode = False, needsMain = False} of -- FIXME: pass on flags
            Left err -> error $ "Parse error: " ++ errorBundlePretty err
            Right (Program exprs) -> exprs
    if qualified || isJust as
        then do
            let alias = if qualified then from else fromJust as
            concatMapM verifyExpr (map (`mangleAST` alias) expr)
        else concatMapM verifyExpr expr
  where
    mangleAST :: Parser.Expr -> String -> Parser.Expr
    mangleAST (Parser.FuncDec name types _) alias = Parser.FuncDec (alias ++ "@" ++ name) types []
    mangleAST (Parser.Function fdef dec) alias = Parser.Function (map (`mangleAST` alias) fdef) (mangleAST dec alias)
    mangleAST (Parser.FuncDef name args body) alias = Parser.FuncDef (alias ++ "@" ++ name) args (mangleAST body alias)
    mangleAST x _ = x
verifyExpr (Parser.Cast _ _) = return [] -- TODO
verifyExpr x = do
    modify (\state -> state{topLevel = False})
    concatMapM verifyExpr (children x)
