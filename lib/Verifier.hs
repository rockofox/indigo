{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Verifier where

import AST
import Control.Monad.Loops (allM)
import Control.Monad.State.Lazy hiding (state)
import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Data.Void
import Parser
import Text.Megaparsec hiding (State)
import Util

data VerifierState = VerifierState {frames :: [VerifierFrame]} deriving (Show)

data VerifierFrame = VerifierFrame
    { bindings :: Set.Set VBinding
    , ttypes :: Map.Map String VType
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
    }
    deriving (Show, Eq, Ord)

findMatchingBinding :: String -> StateT VerifierState IO (Maybe VBinding)
findMatchingBinding name = do
    frames' <- frames <$> get
    let matchingBindings = concatMap (Set.toList . Set.filter (\binding -> Verifier.name binding == name) . bindings) frames'
    if null matchingBindings
        then return Nothing
        else return $ Just $ head matchingBindings

listPatternToBindings :: Expr -> Type -> [VBinding]
listPatternToBindings (ListPattern exprs) (List t) = do
    let singulars = init exprs
    let Var restName _ = last exprs
    map (\(Var name _) -> VBinding{name = name, args = [], ttype = t}) singulars ++ [VBinding{name = restName, args = [], ttype = List t}]
listPatternToBindings _ _ = error "listPatternToBinding called with non-list-pattern"

typeOf' :: Expr -> StateT VerifierState IO Type
typeOf' (Var name _) = do
    matchingBinding <- findMatchingBinding name
    return $ maybe Unknown ttype matchingBinding
typeOf' (FuncCall name _ _) = do
    matchingBinding <- findMatchingBinding name
    return $ maybe Unknown ttype matchingBinding
typeOf' x = return $ typeOf x

compareTypes' :: Type -> Type -> StateT VerifierState IO Bool
compareTypes' (StructT a) (StructT b) = do
    rootFrame <- gets frames <&> last
    let ttypes' = ttypes rootFrame
    let a' = Map.findWithDefault (VType{implements = []}) a ttypes'
    let b' = Map.findWithDefault (VType{implements = []}) b ttypes'
    return $ a `elem` implements b' || b `elem` implements a'
compareTypes' a b = return $ compareTypes a b

functionTypesAcceptable :: [Type] -> [Type] -> StateT VerifierState IO Bool
functionTypesAcceptable use def = allM (uncurry compareTypes') $ zip use def

-- functionTypesAcceptable use def = all (uncurry compareTypes) $ zip use def

argsOf :: Expr -> [Type]
argsOf expr = case typeOf expr of
    Fn args _ -> args
    _ -> []

initVerifierState :: VerifierState
initVerifierState =
    VerifierState
        { frames = [VerifierFrame{bindings = Set.fromList [VBinding{name = "print", args = [Any], ttype = None}, VBinding{name = "println", args = [Any], ttype = None}], ttypes = Map.empty}] -- TODO: actually import prelude
        }

currentFrame :: StateT VerifierState IO VerifierFrame
currentFrame = head . frames <$> get

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
    errors <- concatMapM verifyExpr exprs
    if null errors
        then return $ Right ()
        else return $ Left $ ParseErrorBundle{bundleErrors = NonEmpty.fromList errors, bundlePosState = initialState}

verifyMultiple :: [Expr] -> StateT VerifierState IO [ParseError s e]
verifyMultiple = concatMapM verifyExpr

verifyExpr :: Expr -> StateT VerifierState IO [ParseError s e]
verifyExpr (FuncDef name args body) = do
    -- TODO: Position
    -- currentFrame' <- currentFrame
    -- modify (\state -> state { frames = currentFrame' { bindings = Set.insert (VBinding { name = name, args = map typeOf args, ttype = Any }) (bindings (head (frames state))) } : tail (frames state) })
    b <- findMatchingBinding name
    case b of
        Just binding -> do
            let argsAndTypes = zip args binding.args
            let argsAsBindings = concatMap argToBindings argsAndTypes
            -- let argsAsBindings = concatMap (\case { Var name' _ -> [VBinding { name = name', args = [], ttype = Any }]; l@ListPattern{} -> listPatternToBindings l; _ -> [] }) args
            modify (\state -> state{frames = (VerifierFrame{bindings = Set.fromList argsAsBindings, ttypes = Map.empty}) : frames state})
            -- Add the function itself to the frame
            modify (\state -> state{frames = (VerifierFrame{bindings = Set.insert (VBinding{name = name, args = map typeOf args, ttype = Any}) (bindings (head (frames state))), ttypes = Map.empty}) : tail (frames state)})
            bodyErrors <- verifyExpr body
            modify (\state -> state{frames = tail (frames state)})
            return bodyErrors
          where
            argToBindings :: (Expr, Type) -> [VBinding]
            argToBindings (Var name' _, Fn args' ret) = [VBinding{name = name', args = args', ttype = ret}]
            argToBindings (Var name' _, ttype') = [VBinding{name = name', args = [], ttype = ttype'}]
            argToBindings (l@ListPattern{}, t) = listPatternToBindings l t
            argToBindings _ = []
        Nothing -> return [FancyError 0 (Set.singleton (ErrorFail $ "Function " ++ name ++ " is missing a declaration"))]
verifyExpr (FuncDec name types) = do
    currentFrame' <- currentFrame
    let (arguments, returnType) = if null types then ([], Any) else (init types, last types)
    modify (\state -> state{frames = currentFrame'{bindings = Set.insert (VBinding{name = name, args = arguments, ttype = returnType}) (bindings (head (frames state)))} : tail (frames state)})
    return []
verifyExpr (DoBlock exprs) = verifyMultiple exprs
verifyExpr (Function def dec) = do
    b <- verifyExpr dec
    a <- verifyMultiple def
    return $ a ++ b
verifyExpr (Trait _ methods) = do
    currentFrame' <- currentFrame
    modify (\state -> state{frames = currentFrame'{bindings = Set.union (bindings (head (frames state))) (Set.fromList (map (\(FuncDec name' types) -> VBinding{name = name', args = types, ttype = Any}) methods))} : tail (frames state)})
    return []
verifyExpr (Let name expr) = do
    currentFrame' <- currentFrame
    letType <- typeOf' expr
    modify (\state -> state{frames = currentFrame'{bindings = Set.insert (VBinding{name = name, args = [], ttype = letType}) (bindings (head (frames state)))} : tail (frames state)})
    verifyExpr expr
verifyExpr (Var name (Position (start, _))) = do
    matchingBinding <- findMatchingBinding name
    return [FancyError start (Set.singleton (ErrorFail $ "Could not find relevant binding for " ++ name)) | isNothing matchingBinding]
verifyExpr (StructAccess _ _) = return [] -- TODO
verifyExpr (Impl trait for _) = do
    rootFrame <- head . frames <$> get
    modify (\state -> state{frames = rootFrame{ttypes = Map.alter (\case Just (VType{implements = impls}) -> Just (VType{implements = trait : impls}); Nothing -> Just (VType{implements = [trait]})) for (ttypes rootFrame)} : tail (frames state)})
    -- let ttypes' = ttypes rootFrame
    -- let traitType = Map.findWithDefault (VType { implements = [] }) trait ttypes'
    -- let vtype = VType { implements = traitType.implements ++ [trait]}
    -- modify (\state -> state { frames = rootFrame { ttypes = Map.update (\_ -> Just vtype) for (ttypes rootFrame) } : tail (frames state) })
    return []
verifyExpr (FuncCall name args (Position (start, _))) = do
    matchingBinding <- findMatchingBinding name
    eArgs <- concatMapM verifyExpr args
    argumentTypes <- mapM typeOf' args
    fta <- case matchingBinding of
        Just binding -> functionTypesAcceptable argumentTypes binding.args
        Nothing -> return False
    let eTypes = ([FancyError start (Set.singleton (ErrorFail ("Argument types do not match on " ++ name ++ ", expected: " ++ show (fromJust matchingBinding).args ++ ", got: " ++ show argumentTypes))) | isJust matchingBinding && not fta])
    return $ [FancyError start (Set.singleton (ErrorFail $ "Could not find relevant binding for " ++ name)) | isNothing matchingBinding] ++ eArgs ++ eTypes
verifyExpr (Parser.Import{objects = o, from = from, as = as, qualified = qualified}) = do
    when (o /= ["*"]) $ error "Only * imports are supported right now"
    let convertedPath = map (\x -> if x == '@' then '/' else x) from
    i <- liftIO $ readFile $ convertedPath ++ ".in"
    let expr = case parseProgram (Data.Text.pack i) CompilerFlags{verboseMode = False} of -- FIXME: pass on flags
            Left err -> error $ "Parse error: " ++ errorBundlePretty err
            Right (Program exprs) -> exprs
    -- concatMapM compileExpr (map mangleAST expr)
    -- error $ show (map mangleAST expr)
    if qualified || isJust as
        then do
            let alias = if qualified then from else fromJust as
            concatMapM verifyExpr (map (`mangleAST` alias) expr)
        else concatMapM verifyExpr expr
  where
    mangleAST :: Parser.Expr -> String -> Parser.Expr
    mangleAST (Parser.FuncDec name types) alias = Parser.FuncDec (alias ++ "@" ++ name) types
    mangleAST (Parser.Function fdef dec) alias = Parser.Function (map (`mangleAST` alias) fdef) (mangleAST dec alias)
    mangleAST (Parser.FuncDef name args body) alias = Parser.FuncDef (alias ++ "@" ++ name) args (mangleAST body alias)
    mangleAST x _ = x
verifyExpr x = do
    concatMapM verifyExpr (children x)
