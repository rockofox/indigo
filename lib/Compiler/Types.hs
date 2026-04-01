module Compiler.Types where

import AST qualified as Parser
import AST qualified as Parser.Type (Type (Unknown))
import Compiler.State
    ( Compiler
    , Let (..)
    , implsFor
    , internalError
    )
import Compiler.State qualified as CS (funcDecs, lets, structDecs)
import Control.Monad.State (gets)
import Data.Functor ((<&>))
import Data.List (find, intercalate)
import Data.Map qualified
import Data.Maybe (fromJust)
import Foreign (nullPtr, ptrToWordPtr)
import VM (Data (..), Instruction (..))

typeToString :: Parser.Type -> String
typeToString (Parser.StructT x typeArgs)
    | null typeArgs = x
    | otherwise = x ++ "<" ++ intercalate ", " (map typeToString typeArgs) ++ ">"
typeToString x = show x

typeToData :: Parser.Type -> VM.Data
typeToData (Parser.StructT "Int" []) = VM.DInt 0
typeToData (Parser.StructT "Float" []) = VM.DFloat 0
typeToData (Parser.StructT "Double" []) = VM.DDouble 0
typeToData (Parser.StructT "Bool" []) = VM.DBool False
typeToData (Parser.StructT "String" []) = VM.DString ""
typeToData (Parser.StructT "IO" []) = VM.DNone -- Hmmm...
typeToData (Parser.StructT "Char" []) = VM.DChar ' '
typeToData (Parser.StructT "CPtr" []) = VM.DCPtr 0
typeToData (Parser.StructT _ _) = VM.DMap Data.Map.empty
typeToData Parser.Any = VM.DNone
typeToData x = internalError $ "Cannot convert type " ++ show x ++ " to data"

compileTypeFromType :: Parser.Type -> [Instruction]
compileTypeFromType (Parser.StructT "Int" []) = [Push $ DInt 0]
compileTypeFromType (Parser.StructT "Float" []) = [Push $ DFloat 0.0]
compileTypeFromType (Parser.StructT "Double" []) = [Push $ DDouble 0.0]
compileTypeFromType (Parser.StructT "Bool" []) = [Push $ DBool False]
compileTypeFromType (Parser.StructT "Char" []) = [Push $ DChar '\0']
compileTypeFromType (Parser.StructT "String" []) = [Push $ DString ""]
compileTypeFromType (Parser.StructT "CPtr" []) = [Push $ DCPtr $ ptrToWordPtr nullPtr]
compileTypeFromType _ = [Push DNone]

evaluateFunction :: [Parser.Type] -> Int -> Parser.Type
evaluateFunction types taken = case drop taken types of
    [] -> Parser.Unknown
    [x] -> x
    xs -> Parser.Fn xs (last xs)

allTheSame :: [Parser.Type] -> Bool
allTheSame [] = True
allTheSame [_] = True
allTheSame xs = all (compareSame $ head xs) (tail xs)
  where
    compareSame :: Parser.Type -> Parser.Type -> Bool
    compareSame (Parser.List x) (Parser.List y) = compareSame x y
    compareSame x y = x == y

typeCompatible :: Parser.Type -> Parser.Type -> Compiler Bool
typeCompatible x y = do
    let x' = typeToString x
    let y' = typeToString y
    impls <- implsFor x'
    let equal = Parser.compareTypes x y
    if equal
        then return True
        else do
            let numericTypes = ["Int", "Float", "Double"]
            let bothNumeric = x' `elem` numericTypes && y' `elem` numericTypes
            let boolAliases = ["Bool", "Boolean"]
            let bothBool = x' `elem` boolAliases && y' `elem` boolAliases
            if bothNumeric || bothBool
                then return True
                else return $ y' `elem` impls

compareTypes' :: Parser.Type -> Parser.Type -> [Parser.GenericExpr] -> Compiler Bool
compareTypes' (Parser.List x) (Parser.List y) generics = compareTypes' x y generics
compareTypes' (Parser.List Parser.Any) (Parser.StructT "String" []) _ = return True
compareTypes' (Parser.List (Parser.StructT "Char" [])) (Parser.StructT "String" []) _ = return True
compareTypes' (Parser.StructT "String" []) (Parser.List (Parser.StructT "Char" [])) _ = return True
compareTypes' (Parser.StructT "String" []) (Parser.List Parser.Any) _ = return True
compareTypes' aT (Parser.StructT b bArgs) generics = case aT of
    Parser.StructT a aArgs -> do
        if length aArgs == length bArgs && all (uncurry (==)) (zip aArgs bArgs)
            then do
                let gen = find (\(Parser.GenericExpr name _) -> name == b) generics
                case gen of
                    Just (Parser.GenericExpr _ (Just (Parser.StructT t _))) -> compStructs a t
                    Just (Parser.GenericExpr _ _) -> return True
                    Nothing -> compStructs a b
            else return False
    Parser.Unknown -> return True
    _ -> do
        let gen = find (\(Parser.GenericExpr name _) -> name == b) generics
        case gen of
            Just (Parser.GenericExpr _ (Just t)) -> return $ Parser.compareTypes aT t
            Just (Parser.GenericExpr _ Nothing) -> return True
            Nothing -> return False
  where
    compStructs :: String -> String -> Compiler Bool
    compStructs structA structB = do
        implsA <- implsFor structA
        return $ structB `elem` implsA || structA == structB
compareTypes' a b _ = return $ Parser.compareTypes a b

allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM f = fmap and . mapM f

functionTypesAcceptable :: [Parser.Type] -> [Parser.Type] -> [Parser.GenericExpr] -> Compiler Bool
functionTypesAcceptable use def generics = do
    let genericNames = map (\(Parser.GenericExpr name _) -> name) generics
    let useAndDef = zip use def
    let udStructs = concatMap (\(a, b) -> case (a, b) of (_, Parser.StructT _ _) -> [(a, b)]; (Parser.List c'@(Parser.StructT _ _), Parser.List c@(Parser.StructT _ _)) -> [(c', c)]; _ -> []) useAndDef
    let udStructsGeneric = filter (\case (_, Parser.StructT b _) -> b `elem` genericNames; (_, Parser.List (Parser.StructT b _)) -> b `elem` genericNames; _ -> internalError "Impossible") udStructs
    let groupedUdStructsGeneric = map (\x -> (x, fst <$> filter (\case (_, Parser.StructT b _) -> b == x; (_, Parser.List (Parser.StructT b _)) -> b == x; _ -> internalError "Impossible") udStructsGeneric)) genericNames
    let genericsMatch = all (\(_, types) -> allTheSame types) groupedUdStructsGeneric
    typesMatch' <- allM (uncurry $ uncurry compareTypes') $ zip (zip use def) [generics]
    return $ typesMatch' && genericsMatch

typeOf :: Parser.Expr -> Compiler Parser.Type
typeOf Parser.FuncCall{funcName, funcArgs} = do
    funcDecs' <- gets CS.funcDecs
    let a =
            maybe
                [Parser.Type.Unknown]
                Parser.types
                (find (\x -> x.name == funcName) funcDecs')
    return $ evaluateFunction a (length funcArgs)
typeOf Parser.Var{varName = varName} = do
    lets' <- gets CS.lets
    let a = maybe Parser.Unknown vtype (find (\(Let n _ _) -> n == varName) lets')
    return a
typeOf (Parser.IntLit _ _) = return $ Parser.StructT "Int" []
typeOf (Parser.FloatLit _ _) = return $ Parser.StructT "Float" []
typeOf (Parser.BoolLit _ _) = return $ Parser.StructT "Bool" []
typeOf (Parser.StringLit _ _) = return $ Parser.StructT "String" []
typeOf (Parser.Add x _ _) = typeOf x
typeOf (Parser.Sub x _ _) = typeOf x
typeOf (Parser.Mul x _ _) = typeOf x
typeOf (Parser.Div x _ _) = typeOf x
typeOf (Parser.Power x _ _) = typeOf x
typeOf (Parser.UnaryMinus x _) = typeOf x
typeOf (Parser.Eq{}) = return $ Parser.StructT "Bool" []
typeOf (Parser.Neq{}) = return $ Parser.StructT "Bool" []
typeOf (Parser.Lt{}) = return $ Parser.StructT "Bool" []
typeOf (Parser.Gt{}) = return $ Parser.StructT "Bool" []
typeOf (Parser.Le{}) = return $ Parser.StructT "Bool" []
typeOf (Parser.Ge{}) = return $ Parser.StructT "Bool" []
typeOf (Parser.And{}) = return $ Parser.StructT "Bool" []
typeOf (Parser.Or{}) = return $ Parser.StructT "Bool" []
typeOf (Parser.Not _ _) = return $ Parser.StructT "Bool" []
typeOf (Parser.Placeholder _) = return Parser.Any
typeOf (Parser.Let{}) = internalError "Cannot infer type of let"
typeOf (Parser.If _ b _ _) = typeOf b
typeOf (Parser.FuncDef{}) = internalError "Cannot infer type of function definition"
typeOf x@(Parser.FuncDec{}) = internalError $ "Cannot infer type of function declaration " ++ show x
typeOf (Parser.Function{}) = return Parser.Unknown
typeOf (Parser.DoBlock x _) = if null x then return Parser.None else typeOf $ last x
typeOf (Parser.ExternDec{}) = internalError "Cannot infer type of extern declaration"
typeOf (Parser.Discard _ _) = internalError "Cannot infer type of discard"
typeOf (Parser.Import{}) = internalError "Cannot infer type of import"
typeOf (Parser.Ref _ _) = internalError "Cannot infer type of ref"
typeOf (Parser.Struct{}) = internalError "Cannot infer type of struct"
typeOf (Parser.StructLit x _ typeArgs _) = return $ Parser.StructT x typeArgs
typeOf (Parser.ListLit [Parser.Var{varName}] _) = return $ Parser.List $ Parser.StructT varName []
typeOf (Parser.ListLit x _) = case x of
    [] -> return $ Parser.List Parser.Any
    (y : _) -> typeOf y <&> Parser.List
typeOf (Parser.ArrayAccess{}) = internalError "Cannot infer type of array access"
typeOf (Parser.Modulo x _ _) = typeOf x
typeOf (Parser.Target{}) = internalError "Cannot infer type of target"
typeOf (Parser.ListConcat x _ _) = typeOf x
typeOf (Parser.ListPattern _ _) = return $ Parser.List Parser.Any
typeOf (Parser.StructAccess _ s _) = typeOf s
typeOf (Parser.Pipeline _ b _) = typeOf b
typeOf (Parser.Lambda{lambdaArgs, lambdaBody}) = do
    returnType <- typeOf lambdaBody
    let argTypes = replicate (length lambdaArgs) Parser.Any
    return $ Parser.Fn argTypes returnType
typeOf (Parser.Cast _ (Parser.Var to _) _) = return $ Parser.StructT to []
typeOf (Parser.Cast _ b _) = typeOf b
typeOf (Parser.TypeLit x _) = return x
typeOf (Parser.Flexible x _) = typeOf x
typeOf (Parser.Trait{}) = internalError "Cannot infer type of trait"
typeOf (Parser.Impl{}) = internalError "Cannot infer type of impl"
typeOf (Parser.Then _ b _) = typeOf b
typeOf (Parser.StrictEval x _) = typeOf x
typeOf (Parser.External{}) = internalError "Cannot infer type of external"
typeOf (Parser.CharLit _ _) = return $ Parser.StructT "Char" []
typeOf (Parser.DoubleLit _ _) = return $ Parser.StructT "Double" []
typeOf (Parser.ParenApply a _ _) = typeOf a
typeOf (Parser.ListAdd x _ _) = typeOf x
typeOf (Parser.When _ branches else_ _) =
    case branches of
        [] -> maybe (return Parser.Unknown) typeOf else_
        ((_, body) : _) -> typeOf body
typeOf (Parser.TupleLit exprs _) = mapM typeOf exprs <&> Parser.Tuple
typeOf (Parser.TupleAccess tupleExpr index _) = do
    tupleType <- typeOf tupleExpr
    case tupleType of
        Parser.Tuple types -> if index >= 0 && index < length types then return $ types !! index else return Parser.Unknown
        _ -> return Parser.Unknown

{- | Like 'typeOf' but returns 'Parser.Unknown' for declaration-like expressions
that don't have a meaningful type.
-}
safeTypeOf :: Parser.Expr -> Compiler Parser.Type
safeTypeOf (Parser.Let{}) = return Parser.Unknown
safeTypeOf (Parser.FuncDef{}) = return Parser.Unknown
safeTypeOf (Parser.FuncDec{}) = return Parser.Unknown
safeTypeOf (Parser.ExternDec{}) = return Parser.Unknown
safeTypeOf (Parser.Discard{}) = return Parser.Unknown
safeTypeOf (Parser.Import{}) = return Parser.Unknown
safeTypeOf (Parser.Ref{}) = return Parser.Unknown
safeTypeOf (Parser.Struct{}) = return Parser.Unknown
safeTypeOf (Parser.ArrayAccess{}) = return Parser.Unknown
safeTypeOf (Parser.Target{}) = return Parser.Unknown
safeTypeOf (Parser.Trait{}) = return Parser.Unknown
safeTypeOf (Parser.Impl{}) = return Parser.Unknown
safeTypeOf (Parser.External{}) = return Parser.Unknown
safeTypeOf (Parser.DoBlock exprs _) = if null exprs then return Parser.None else safeTypeOf $ last exprs
safeTypeOf (Parser.If _ b _ _) = safeTypeOf b
safeTypeOf (Parser.When _ branches else_ _) = case branches of
    [] -> maybe (return Parser.Unknown) safeTypeOf else_
    ((_, body) : _) -> safeTypeOf body
safeTypeOf expr = typeOf expr

getStructFields :: String -> Compiler [(String, Parser.Type)]
getStructFields structName = do
    structDecs' <- gets CS.structDecs
    let structDec = fromJust $ find (\x -> Parser.name x == structName) structDecs'
    let fields = case structDec of
            Parser.Struct{fields = fields'} -> fields'
            _ -> internalError "Not a struct"
    return fields
