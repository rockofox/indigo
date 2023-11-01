module Verifier where

import AST
    ( Expr (..)
    , PosExpr (..)
    , Type (..)
    , typeOf
    , typesMatch
    )
import Control.Monad.State (State, evalState, gets, modify)
import Data.List (find)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (ErrorFancy (..), ParseError (..), ParseErrorBundle (..), PosState (..))
import Text.Megaparsec.Pos (defaultTabWidth, initialPos)

data VFn = VFn
    { name :: String
    , fpos :: Int
    , scope :: Maybe String
    , args :: [Type]
    }
    deriving (Eq, Ord, Show)

data VerifierState = VerifierState
    { frames :: [VerifierFrame]
    , usedFunctions :: Set.Set VFn
    , definedFunctions :: Set.Set VFn
    , vpos :: (Int, Int)
    , program :: [PosExpr]
    }
    deriving (Show)

newtype VLet = VLet (String, Type) deriving (Show)

data VerifierFrame = VerifierFrame
    { name :: String
    , lets :: [VLet]
    }
    deriving (Show)

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

topFrame :: State VerifierState VerifierFrame
topFrame = do
    frames' <- gets frames
    return $ head frames'

preludeFunctions :: [VFn]
preludeFunctions =
    -- TODO: Import prelude instead
    [ VFn "print" 0 Nothing [Any, IO]
    , VFn "println" 0 Nothing [Any, IO]
    ]

verifyProgram :: String -> Text -> [PosExpr] -> Either (ParseErrorBundle Text Void) ()
verifyProgram name input exprs = evalState (verifyProgram' name input exprs) VerifierState{frames = [VerifierFrame{name = "__outside__", lets = []}], usedFunctions = Set.empty, definedFunctions = Set.fromList preludeFunctions, vpos = (0, 0), program = exprs}

verifyProgram' :: String -> Text -> [PosExpr] -> State VerifierState (Either (ParseErrorBundle Text Void) ())
verifyProgram' name input exprs = do
    let initialState =
            PosState
                { pstateInput = input
                , pstateOffset = 0
                , pstateSourcePos = initialPos name
                , pstateTabWidth = defaultTabWidth
                , pstateLinePrefix = ""
                }
    -- error $ T.unpack $ pShow exprs
    _ <- pass1 (filter (\(PosExpr (expr, _, _)) -> case expr of FuncDec{} -> True; _ -> False) exprs)
    errors2 <- pass1 (filter (\(PosExpr (expr, _, _)) -> case expr of FuncDec{} -> False; _ -> True) exprs)
    errors3 <- verifyFunctionUsage
    let errors = errors2 ++ errors3
    if null errors
        then return $ Right ()
        else return $ Left $ ParseErrorBundle{bundleErrors = NonEmpty.fromList errors, bundlePosState = initialState}
  where
    pass1 :: [PosExpr] -> State VerifierState [ParseError Text Void]
    pass1 (PosExpr (expr, start, end) : xs) = do
        modify (\state -> (state :: VerifierState){vpos = (start, end)})
        pass1Expr expr >>= \case
            Nothing -> pass1 xs
            Just err -> do
                errors <- pass1 xs
                return $ FancyError start (Set.singleton (ErrorFail err)) : errors
    pass1 _ = return []

handleMultipleErrors :: [Maybe [Char]] -> Maybe [Char]
handleMultipleErrors [] = Nothing
handleMultipleErrors (Nothing : xs) = handleMultipleErrors xs
handleMultipleErrors (Just err : _) = Just err

pass1Expr :: Expr -> State VerifierState (Maybe [Char])
pass1Expr (FuncDef{name = name', args = args', body = _}) = do
    pos' <- gets vpos
    definedFunctions' <- gets definedFunctions
    let decl = find (\(VFn name'' _ _ _) -> name'' == name') definedFunctions'
    let argTypes = case decl of
            Just (VFn _ _ _ argTypes') -> take (length args') argTypes'
            Nothing -> replicate (length args') Any
    let processedArgs = processArgs args' argTypes
    let argFns = map (\(x, y) -> VFn x (fst pos') (Just name') case y of Fn{args} -> args; _ -> []) processedArgs
    if isNothing decl
        then modify (\state -> state{frames = VerifierFrame{name = name', lets = map VLet processedArgs} : frames state, definedFunctions = Set.insert (VFn name' (fst (vpos state)) Nothing (replicate (length args') Any)) (definedFunctions state)})
        else modify (\state -> state{frames = VerifierFrame{name = name', lets = map VLet processedArgs} : frames state})
    mapM_ (\x -> modify (\state -> state{definedFunctions = Set.insert x (definedFunctions state)})) argFns
    return Nothing
  where
    -- Handle list patterns. (x:xs) -> [("x", Int), ("xs", List Int)]
    processArgs :: [Expr] -> [Type] -> [(String, Type)]
    processArgs [] [] = []
    processArgs (Var varName : xs) (argType : ys) = (varName, argType) : processArgs xs ys
    processArgs (ListPattern lNames : xs) (List argType : ys) = handleListPattern (ListPattern lNames) argType ++ processArgs xs ys
    processArgs (ListLit [] : _) _ = []
    processArgs e t = error $ "Invalid function definition: " ++ show e ++ " " ++ show t

    handleListPattern :: Expr -> Type -> [(String, Type)]
    handleListPattern (ListPattern names) ttype = zip names'' (replicate (length names'') ttype) ++ [(rest, List ttype)]
      where
        names' = map (\(Var x) -> x) names
        names'' = take (length names' - 1) names'
        rest = last names'
    handleListPattern e t = error $ "Invalid list pattern: " ++ show e ++ " " ++ show t
pass1Expr (FuncDec name types) = do
    pos' <- gets vpos
    modify (\state -> state{definedFunctions = Set.insert (VFn{name = name, fpos = fst pos', scope = Nothing, args = types}) (definedFunctions state)})
    return Nothing
pass1Expr (Function fdef fdec) = do
    mapM_ pass1Expr fdef
    _ <- pass1Expr fdec
    return Nothing
pass1Expr (DoBlock _) = do
    return Nothing
pass1Expr (Let letName letVal) = do
    frame <- topFrame
    if letName `elem` map (\(VLet (x, _)) -> x) (lets frame)
        then return $ Just $ "Variable " ++ letName ++ " already defined in this scope"
        else do
            modify (\state -> state{frames = frame{lets = VLet (letName, typeOf letVal) : lets frame} : tail (frames state)})
            return Nothing
pass1Expr (Var varName) = do
    frame <- topFrame
    if varName `elem` map (\(VLet (x, _)) -> x) (lets frame)
        then return Nothing
        else return $ Just $ "Variable " ++ varName ++ " not defined in this scope"
pass1Expr (Add x y) = do
    x' <- pass1Expr x
    y' <- pass1Expr y
    return $ handleMultipleErrors [x', y']
pass1Expr (FuncCall name' args') = do
    pos' <- gets vpos
    frame' <- topFrame
    let calledIn = frame'.name
    args'' <- mapM typeOf' args'
    modify (\state -> state{usedFunctions = Set.insert (VFn{name = name', fpos = fst pos', scope = Just calledIn, args = args''}) (usedFunctions state)})
    return Nothing
  where
    typeOf' :: Expr -> State VerifierState Type
    typeOf' (FuncCall{}) = do
        definedFunctions <- gets definedFunctions
        -- error $ T.unpack $ pShow definedFunctions
        return $ case find (\(VFn name'' _ _ _) -> name' == name'') definedFunctions of
            Just (VFn _ _ _ args'') -> last args''
            Nothing -> Unknown
    typeOf' (Var varName) = do
        frame <- topFrame
        let lets' = map (\(VLet (x, y)) -> (x, y)) (lets frame)
        return $ fromMaybe Unknown (lookup varName lets')
    typeOf' x = return $ typeOf x
pass1Expr (Impl _ _ methods') = do
    let methods'' = [x | x@FuncDef{} <- methods']
    modify (\state -> state{definedFunctions = Set.union (Set.fromList [VFn method.name (fst (vpos state)) Nothing (replicate (length method.args + 1) Any) | method <- methods'']) (definedFunctions state)}) -- TODO: Types, use Trait instead
    return Nothing
pass1Expr _ = return Nothing

verifyFunctionUsage :: State VerifierState [ParseError Text Void]
verifyFunctionUsage = do
    usedFunctions' <- gets usedFunctions
    definedFunctions' <- gets definedFunctions
    let undefinedFunctions = filter (\(VFn vfnName _ scope' _) -> not $ any (\(VFn name' _ scope'' _) -> vfnName == name' && (scope' == scope'' || isNothing scope'')) definedFunctions') (Set.toList usedFunctions')
    let usedAndDefined = filter (\(VFn vfnName _ _ _) -> any (\(VFn name' _ _ _) -> vfnName == name') definedFunctions') (Set.toList usedFunctions')
    let wrongTypesFunctions = filter (\(VFn vfnName _ _ callArgs) -> not $ any (\(VFn name' _ _ fnArgs) -> {- trace (show args' ++ "," ++ show args'')  -} vfnName == name' && typesMatch callArgs (init fnArgs)) definedFunctions') usedAndDefined
    let notDefinedErrors = map (\(VFn vfnName pos' _ _) -> FancyError pos' (Set.singleton (ErrorFail $ "Function " ++ vfnName ++ " not defined"))) undefinedFunctions
    let wrongTypesErrors = map (\(VFn vfnName pos' _ _) -> FancyError pos' (Set.singleton (ErrorFail $ "Function " ++ vfnName ++ " called with wrong types. Expected " ++ show (init (findVFnArgs vfnName definedFunctions')) ++ ", got " ++ show (findVFnArgs vfnName usedFunctions')))) wrongTypesFunctions -- TODO: Make sure it shosw correct types (maybe it needs `init`)
    return $ notDefinedErrors ++ wrongTypesErrors
  where
    findVFnArgs :: String -> Set.Set VFn -> [Type]
    findVFnArgs name' fns = case find (\(VFn name'' _ _ _) -> name' == name'') fns of
        Just (VFn _ _ _ args') -> args'
        Nothing -> []
