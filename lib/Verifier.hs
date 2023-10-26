module Verifier where

import AST
    ( Expr(..) )
import Control.Monad.State ( modify, evalState, gets, State )
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (ErrorFancy (..), ParseError (..), ParseErrorBundle (..), PosState (..))
import Text.Megaparsec.Pos ( defaultTabWidth, initialPos )

data VerifierState = VerifierState
    { frames :: [VerifierFrame]
    , usedFunctions :: Set.Set (String, Int)
    , definedFunctions :: Set.Set (String, Int)
    , pos :: (Int, Int)
    }
    deriving (Show)

data VerifierFrame = VerifierFrame
    { fname :: String
    , lets :: [String]
    }
    deriving (Show)

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

topFrame :: State VerifierState VerifierFrame
topFrame = do
    frames <- gets frames
    return $ head frames

preludeFunctions :: [(String, Int)]
preludeFunctions =
    [ ("print", 0)
    , ("println", 0)]
verifyProgram :: Text -> [(Expr, Int, Int)] -> Either (ParseErrorBundle Text Void) ()
verifyProgram input exprs = evalState (verifyProgram' input exprs) VerifierState{frames = [VerifierFrame{fname = "__outside__", lets = []}], usedFunctions = Set.empty, definedFunctions = Set.fromList preludeFunctions, pos = (0, 0)}

verifyProgram' :: Text -> [(Expr, Int, Int)] -> State VerifierState (Either (ParseErrorBundle Text Void) ())
verifyProgram' input exprs = do
    let initialState =
            PosState
                { pstateInput = input
                , pstateOffset = 0
                , pstateSourcePos = initialPos ""
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
        modify (\state -> state{pos = (start, end)})
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
pass1Expr (Function fdec fdef) = do
    pass1Expr fdef
pass1Expr (FuncDef{fname = fname, fargs = fargs, fbody = fbody}) = do
    let lets' = [name | Var name <- fargs]
    modify (\state -> state{frames = VerifierFrame{fname = fname, lets = lets'} : frames state, definedFunctions = Set.insert (fname, fst (pos state)) (definedFunctions state)})
    -- verifyExpr fbody
    return Nothing
pass1Expr (DoBlock exprs) = do
    -- mapM verifyExpr exprs >>= \case
    --     [] -> return Nothing
    --     errors -> return $ head errors
    return Nothing
pass1Expr (Let name _) = do
    frame <- topFrame
    if name `elem` lets frame
        then return $ Just $ "Variable " ++ name ++ " already defined in this scope"
        else do
            modify (\state -> state{frames = frame{lets = name : lets frame} : tail (frames state)})
            return Nothing
pass1Expr (Var name) = do
    frame <- topFrame
    if name `elem` lets frame
        then return Nothing
        else return $ Just $ "Variable " ++ name ++ " not defined in this scope"
pass1Expr (Add x y) = do
    x' <- pass1Expr x
    y' <- pass1Expr y
    return $ handleMultipleErrors [x', y']
pass1Expr (FuncCall name _) = do
    pos <- gets pos
    modify (\state -> state{usedFunctions = Set.insert (name, fst pos) (usedFunctions state)})
    return Nothing
pass1Expr _ = return Nothing

verifyFunctionUsage :: State VerifierState [ParseError Text Void]
verifyFunctionUsage = do
    usedFunctions' <- gets usedFunctions
    definedFunctions' <- gets definedFunctions
    let undefinedFunctions = filter (\(name, _) -> not $ any (\(name',_) -> name == name') definedFunctions') (Set.toList usedFunctions')
    return $ map (\(name, pos) -> FancyError pos (Set.singleton (ErrorFail $ "Function " ++ name ++ " not defined")) ) undefinedFunctions
