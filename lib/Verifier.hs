module Verifier where

import AST
    ( Expr (..)
    )
import Control.Monad.State (State, evalState, gets, modify)
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
    }
    deriving (Eq, Ord, Show)

data VerifierState = VerifierState
    { frames :: [VerifierFrame]
    , usedFunctions :: Set.Set VFn
    , definedFunctions :: Set.Set VFn
    , vpos :: (Int, Int)
    }
    deriving (Show)

data VerifierFrame = VerifierFrame
    { name :: String
    , lets :: [String]
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
    [ VFn "print" 0 Nothing
    , VFn "println" 0 Nothing
    ]

verifyProgram :: String -> Text -> [(Expr, Int, Int)] -> Either (ParseErrorBundle Text Void) ()
verifyProgram name input exprs = evalState (verifyProgram' name input exprs) VerifierState{frames = [VerifierFrame{name = "__outside__", lets = []}], usedFunctions = Set.empty, definedFunctions = Set.fromList preludeFunctions, vpos = (0, 0)}

verifyProgram' :: String -> Text -> [(Expr, Int, Int)] -> State VerifierState (Either (ParseErrorBundle Text Void) ())
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
    errors1 <- pass1 exprs
    errors2 <- verifyFunctionUsage
    let errors = errors1 ++ errors2
    if null errors
        then return $ Right ()
        else return $ Left $ ParseErrorBundle{bundleErrors = NonEmpty.fromList errors, bundlePosState = initialState}
  where
    pass1 :: [(Expr, Int, Int)] -> State VerifierState [ParseError Text Void]
    pass1 ((expr, start, end) : xs) = do
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
pass1Expr (Function _ fdef') = do
    pass1Expr fdef'
pass1Expr (FuncDef{name = name', args = args', body = _}) = do
    pos' <- gets vpos
    let lets' = [varName | Var varName <- args']
    let patterns = [varName | ListPattern names <- args', Var varName <- names]
    let lets'' = lets' ++ patterns
    let letFns = map (\x -> VFn x (fst pos') (Just name')) lets''
    modify (\state -> state{frames = VerifierFrame{name = name', lets = lets''} : frames state, definedFunctions = Set.insert (VFn name' (fst (vpos state)) Nothing) (definedFunctions state)})
    mapM_ (\x -> modify (\state -> state{definedFunctions = Set.insert x (definedFunctions state)})) letFns
    -- modify (\state -> state{definedFunctions = Set.union (usedFunctions state) (Set.fromList letFns)})
    -- verifyExpr body
    return Nothing
pass1Expr (DoBlock _) = do
    -- mapM verifyExpr exprs >>= \case
    --     [] -> return Nothing
    --     errors -> return $ head errors
    return Nothing
pass1Expr (Let letName _) = do
    frame <- topFrame
    if letName `elem` lets frame
        then return $ Just $ "Variable " ++ letName ++ " already defined in this scope"
        else do
            modify (\state -> state{frames = frame{lets = letName : lets frame} : tail (frames state)})
            return Nothing
pass1Expr (Var varName) = do
    frame <- topFrame
    if varName `elem` lets frame
        then return Nothing
        else return $ Just $ "Variable " ++ varName ++ " not defined in this scope"
pass1Expr (Add x y) = do
    x' <- pass1Expr x
    y' <- pass1Expr y
    return $ handleMultipleErrors [x', y']
pass1Expr (FuncCall name' _) = do
    pos' <- gets vpos
    frame' <- topFrame
    let calledIn = frame'.name
    modify (\state -> state{usedFunctions = Set.insert (VFn name' (fst pos') (Just calledIn)) (usedFunctions state)})
    return Nothing
pass1Expr (Impl _ _ methods') = do
    let methods'' = [x | x@FuncDef{} <- methods']
    let methods''' = map AST.name methods''
    modify (\state -> state{definedFunctions = Set.union (Set.fromList [VFn methodName (fst (vpos state)) Nothing | methodName <- methods''']) (definedFunctions state)})
    return Nothing
pass1Expr _ = return Nothing

verifyFunctionUsage :: State VerifierState [ParseError Text Void]
verifyFunctionUsage = do
    usedFunctions' <- gets usedFunctions
    definedFunctions' <- gets definedFunctions
    let undefinedFunctions = filter (\(VFn vfnName _ scope') -> not $ any (\(VFn name' _ scope'') -> vfnName == name' && (scope' == scope'' || isNothing scope'')) definedFunctions') (Set.toList usedFunctions')
    return $ map (\(VFn vfnName pos' _) -> FancyError pos' (Set.singleton (ErrorFail $ "Function " ++ vfnName ++ " not defined"))) undefinedFunctions
