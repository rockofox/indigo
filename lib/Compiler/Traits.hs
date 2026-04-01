module Compiler.Traits where

import AST (anyPosition)
import AST qualified as Parser
import Compiler.State
    ( CompileExpr
    , Compiler
    , CompilerState (..)
    , Function (..)
    , cerror
    , implsFor
    , internalError
    , structNameFromType
    )
import Compiler.Structs (substituteGenerics, validateTypeParameters)
import Compiler.Types (typeToString)
import Control.Monad (forM_, unless, when)
import Control.Monad.State (gets, modify)
import Data.Bifunctor (second)
import Data.List (find, intercalate)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, isJust)
import VM (Action (..), Data (..), Instruction (..))

compileTrait :: CompileExpr -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
compileTrait compileExpr' (Parser.Trait{name, methods, generics, requiredProperties, refinement, refinementSrc}) expectedType = do
    let methods' = map (\case Parser.FuncDec{name = name', types} -> Parser.FuncDec{name = name', types = types, generics = [], funcDecPos = anyPosition}; _ -> internalError "Expected FuncDec in Trait methods") methods
    modify (\s -> s{traits = Parser.Trait{name = name, methods = methods, generics = generics, requiredProperties = requiredProperties, refinement = refinement, refinementSrc = refinementSrc, traitPos = anyPosition} : traits s})
    mapM_ (`compileExpr'` expectedType) methods'
    return []
compileTrait _ _ _ = internalError "compileTrait: expected Trait"

compileImpl :: CompileExpr -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
compileImpl compileExpr' impl@(Parser.Impl{trait, traitTypeArgs, for, methods, implPos}) expectedType = do
    let forStructName = structNameFromType for
    structDecs' <- gets structDecs
    let forStructGenerics = case find (\case Parser.Struct{name = name'} -> name' == forStructName; _ -> False) structDecs' of
            Just (Parser.Struct{generics = gs}) -> map (\(Parser.GenericExpr n _) -> n) gs
            _ -> []
    traits' <- gets traits
    trait' <- case find (\x -> Parser.name x == trait) traits' of
        Just t -> return t
        Nothing -> do
            cerror ("Trait '" ++ trait ++ "' not found. Available traits: " ++ intercalate ", " (map Parser.name traits')) implPos
            return $ Parser.Trait{name = trait, methods = [], generics = [], requiredProperties = [], refinement = Nothing, refinementSrc = "", traitPos = implPos}
    let traitGenerics = Parser.generics trait'
    let traitRequiredProps = Parser.requiredProperties trait'
    let _traitRefinement = Parser.refinement trait'
    let _traitRefinementSrc = Parser.refinementSrc trait'
    when (not (null traitGenerics) && length traitTypeArgs /= length traitGenerics) $ do
        cerror ("Trait type argument count mismatch for impl " ++ trait ++ " for " ++ typeToString for ++ ": expected " ++ show (length traitGenerics) ++ ", got " ++ show (length traitTypeArgs)) implPos
    when (null traitGenerics && not (null traitTypeArgs)) $ do
        cerror ("Trait '" ++ trait ++ "' has no type parameters, but impl provides type arguments") implPos
    forM_ traitTypeArgs $ \typeArg -> do
        validateTypeParameters forStructGenerics typeArg implPos ("impl " ++ trait ++ " for " ++ typeToString for)
    let genericSubstitutions =
            if null traitGenerics || null traitTypeArgs
                then []
                else zip (map (\(Parser.GenericExpr n _) -> n) traitGenerics) traitTypeArgs
    structDecs'' <- gets structDecs
    forStruct <- case find (\case Parser.Struct{name = name'} -> name' == forStructName; _ -> False) structDecs'' of
        Just s -> return $ Just s
        Nothing -> return Nothing
    unless (null traitRequiredProps) $ do
        case forStruct of
            Just (Parser.Struct{fields = structFields, generics = structGenerics}) -> do
                let _structGenericNames = map (\(Parser.GenericExpr n _) -> n) structGenerics
                let substitutedRequiredProps =
                        if null genericSubstitutions
                            then traitRequiredProps
                            else map (second (`substituteGenerics` genericSubstitutions)) traitRequiredProps
                let structFieldMap = Map.fromList structFields
                forM_ substitutedRequiredProps $ \(reqName, reqType) -> do
                    case Map.lookup reqName structFieldMap of
                        Just structType -> do
                            unless (Parser.compareTypes reqType structType) $ do
                                cerror ("Required property '" ++ reqName ++ "' type mismatch in impl " ++ trait ++ " for " ++ typeToString for ++ ": trait requires " ++ show reqType ++ ", type has " ++ show structType) implPos
                        Nothing -> do
                            cerror ("Missing required property '" ++ reqName ++ "' in type " ++ forStructName ++ " for trait " ++ trait) implPos
            Nothing -> do
                cerror ("Cannot validate required properties: type " ++ forStructName ++ " not found") implPos
    when (isJust (Parser.refinement trait')) $ do
        case forStruct of
            Just (Parser.Struct{refinement = structRefinement}) -> do
                case structRefinement of
                    Just _ -> return ()
                    Nothing -> return ()
            Nothing -> return ()
    methods' <-
        catMaybes
            <$> mapM
                ( \case
                    Parser.FuncDef{name = name', args, body, funcDefPos} -> do
                        let traitTypeArgsStr = if null traitTypeArgs then "" else "<" ++ intercalate "," (map typeToString traitTypeArgs) ++ ">"
                        let forTypeStr = typeToString for
                        let fullyQualifiedName = trait ++ traitTypeArgsStr ++ "." ++ forTypeStr ++ "::" ++ name'
                        let dec = find (\x -> Parser.name x == name') (Parser.methods trait')
                        case dec of
                            Nothing -> do
                                cerror ("Method '" ++ name' ++ "' is not declared in trait '" ++ trait ++ "'") funcDefPos
                                return Nothing
                            Just dec' -> do
                                let substitutedTypes =
                                        if null genericSubstitutions
                                            then Parser.types dec'
                                            else map (`substituteGenerics` genericSubstitutions) (Parser.types dec')
                                let newDec = Parser.FuncDec{name = fullyQualifiedName, types = unself substitutedTypes for, generics = Parser.generics dec', funcDecPos = anyPosition}
                                _ <- compileExpr' newDec Parser.Any
                                modify (\s -> s{functionsByTrait = (forStructName, name', fullyQualifiedName, newDec) : functionsByTrait s})
                                return $ Just $ Parser.FuncDef{name = fullyQualifiedName, args = args, body = body, funcDefPos = anyPosition}
                    _ -> internalError "Expected FuncDef in Impl methods"
                )
                methods
    modify (\s -> s{impls = impl : impls s})
    mapM_ (`compileExpr'` expectedType) methods'
    return []
  where
    unself :: [Parser.Type] -> Parser.Type -> [Parser.Type]
    unself types selfType = map (\case Parser.Self -> selfType; x -> x) types
compileImpl _ _ _ = internalError "compileImpl: expected Impl"

createVirtualFunctions :: Compiler ()
createVirtualFunctions = do
    impls' <- gets impls
    traits' <- gets traits
    let traitsAssoc = map (\x -> (x, filter (\y -> Parser.trait y == Parser.name x) impls')) traits'
    mapM_ compileTraitVirtual traitsAssoc
  where
    compileTraitVirtual :: (Parser.Expr, [Parser.Expr]) -> Compiler ()
    compileTraitVirtual (traitExpr, implExprs) = do
        let methods = Parser.methods traitExpr
        let fors = map Parser.for implExprs
        mapM_ (\method -> createBaseDef method traitExpr fors) methods
    createBaseDef :: Parser.Expr -> Parser.Expr -> [Parser.Type] -> Compiler ()
    createBaseDef (Parser.FuncDec{name, types = typess}) traitExpr fors = do
        let traitName = Parser.name traitExpr
        impls' <- gets impls
        let funame' = name ++ "#0"
        let body =
                Label funame'
                    : concatMap
                        ( \forType ->
                            let forStructName = structNameFromType forType
                                implForFor = find (\implExpr -> structNameFromType (Parser.for implExpr) == forStructName && Parser.trait implExpr == traitName) impls'
                             in case implForFor of
                                    Just implExpr ->
                                        let traitTypeArgs = Parser.traitTypeArgs implExpr
                                            traitTypeArgsStr = if null traitTypeArgs then "" else "<" ++ intercalate "," (map typeToString traitTypeArgs) ++ ">"
                                            implForTypeStr = typeToString (Parser.for implExpr)
                                         in [PackList (length typess - 2), LStore "__ts", Dup, Push $ DTypeQuery forStructName, TypeEq, LStore "__ta", LLoad "__ts", UnpackList, LLoad "__ta", Jt (traitName ++ traitTypeArgsStr ++ "." ++ implForTypeStr ++ "::" ++ name)]
                                    Nothing -> []
                        )
                        fors
                    ++ [Push $ DString $ "\npanic: No matching implementation of " ++ traitName ++ ", tried calling " ++ name ++ "\n", Builtin Print, Exit]
        modify (\s -> s{functions = functions s ++ [Function name funame' body typess "__outside"]})
        return ()
    createBaseDef _ _ _ = return ()
