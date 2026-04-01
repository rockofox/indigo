module Compiler.Structs where

-- Note: TupleLit / TupleAccess are handled in Compiler.Lists — not duplicated here.

import AST (anyPosition, zeroPosition)
import AST qualified as Parser
import Compiler.Refinements (CompileProgram, runRefinement, transformRefinementExpr)
import Compiler.State
    ( CompileExpr
    , Compiler
    , CompilerState (..)
    , cerror
    , implsFor
    , internalError
    )
import Compiler.State qualified as CS
    ( skipRefinementCheck
    , structDecs
    , traits
    )
import Compiler.Types (typeOf, typeToString)
import Control.Monad (forM_, unless, when)
import Control.Monad.State (gets, modify)
import Data.Bifunctor (second)
import Data.List (find)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Foreign (nullPtr, ptrToWordPtr)
import VM (Data (..), Instruction (..))

substituteGenerics :: Parser.Type -> [(String, Parser.Type)] -> Parser.Type
substituteGenerics (Parser.StructT name typeArgs) substitutions =
    case lookup name substitutions of
        Just subType -> subType
        Nothing -> Parser.StructT name (map (`substituteGenerics` substitutions) typeArgs)
substituteGenerics (Parser.List t) substitutions = Parser.List (substituteGenerics t substitutions)
substituteGenerics (Parser.Tuple ts) substitutions = Parser.Tuple (map (`substituteGenerics` substitutions) ts)
substituteGenerics (Parser.Fn args ret) substitutions = Parser.Fn (map (`substituteGenerics` substitutions) args) (substituteGenerics ret substitutions)
substituteGenerics t _ = t

validateTypeParameters :: [String] -> Parser.Type -> Parser.Position -> String -> Compiler ()
validateTypeParameters validGenericNames typeArg pos context = do
    structDecs' <- gets CS.structDecs
    traits' <- gets CS.traits
    let knownStructNames = map (\case Parser.Struct{name = n} -> n; _ -> "") structDecs'
    let knownTraitNames = map (\case Parser.Trait{name = n} -> n; _ -> "") traits'
    let knownTypeNames = knownStructNames ++ knownTraitNames
    let builtInTypes = ["Int", "Float", "Double", "Bool", "String", "Char", "CPtr", "None"]
    let validateTypeArg typeArg' = case typeArg' of
            Parser.Any -> True
            Parser.Unknown -> True
            Parser.None -> True
            Parser.Self -> True
            Parser.StructT argName [] -> argName `elem` validGenericNames || argName `elem` builtInTypes || argName `elem` knownTypeNames
            Parser.StructT _ nestedArgs -> all validateTypeArg nestedArgs
            Parser.List nested -> validateTypeArg nested
            Parser.Tuple nested -> all validateTypeArg nested
            Parser.Fn args ret -> all validateTypeArg (args ++ [ret])
    unless (validateTypeArg typeArg) $ do
        let typeArgStr = case typeArg of
                Parser.StructT name' [] -> name'
                _ -> typeToString typeArg
        cerror ("Invalid type parameter '" ++ typeArgStr ++ "' in " ++ context ++ ". Type parameters must be either concrete types or match valid generic parameters: " ++ show validGenericNames) pos

compileStruct :: CompileExpr -> CompileProgram -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
compileStruct compileExpr' _compileProgram' st@(Parser.Struct{name = structName, fields, is, generics, structPos}) expectedType = do
    modify (\s -> s{structDecs = st : structDecs s})
    forM_ fields createFieldTrait
    let structGenericNames = map (\(Parser.GenericExpr n _) -> n) generics
    mapM_
        ( \t -> do
            let (traitName, traitTypeArgs) = case t of
                    Parser.StructT name args -> (name, args)
                    _ -> internalError "Expected StructT in is clause"
            forM_ traitTypeArgs $ \typeArg -> do
                validateTypeParameters structGenericNames typeArg structPos ("trait " ++ traitName ++ " for type " ++ structName)
            _ <- compileExpr' (Parser.Impl{trait = traitName, traitTypeArgs = traitTypeArgs, for = Parser.StructT structName [], methods = [], implPos = structPos}) expectedType
            return ()
        )
        is
    return []
  where
    createFieldTrait :: (String, Parser.Type) -> Compiler ()
    createFieldTrait (name, _) = do
        let traitName = "__field_" ++ name
        let trait = Parser.Trait{name = traitName, methods = [Parser.FuncDec{Parser.name = name, Parser.types = [Parser.Self, Parser.Any], Parser.generics = [], funcDecPos = anyPosition}], generics = [], requiredProperties = [], refinement = Nothing, refinementSrc = "", traitPos = anyPosition}
        let impl = Parser.Impl{trait = traitName, traitTypeArgs = [], for = Parser.StructT (Parser.name st) [], methods = [Parser.FuncDef{name = name, args = [Parser.Var{varName = "self", varPos = zeroPosition}], body = Parser.StructAccess{structAccessStruct = Parser.Var{varName = "self", varPos = zeroPosition}, structAccessField = Parser.Var{varName = name, varPos = zeroPosition}, structAccessPos = anyPosition}, funcDefPos = anyPosition}], implPos = anyPosition}
        _ <- compileExpr' trait Parser.Any
        _ <- compileExpr' impl Parser.Any
        return ()
compileStruct _ _ expr _ = internalError $ "compileStruct: unexpected " ++ show expr

compileStructLit :: CompileExpr -> CompileProgram -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
compileStructLit compileExpr' compileProgram' sl@(Parser.StructLit{structLitName, structLitFields, structLitTypeArgs, structLitPos}) _ = do
    skipCheck <- gets CS.skipRefinementCheck
    structDecs' <- gets CS.structDecs
    let struct = find (\case Parser.Struct{name = name'} -> name' == structLitName; _ -> False) structDecs'
    case struct of
        Just expr -> case expr of
            Parser.Struct{fields = declaredFields, generics = structGenerics} -> do
                when (not (null structGenerics) && length structLitTypeArgs /= length structGenerics) $ do
                    cerror ("Type argument count mismatch for type " ++ structLitName ++ ": expected " ++ show (length structGenerics) ++ ", got " ++ show (length structLitTypeArgs)) structLitPos
                let genericSubstitutions =
                        if null structGenerics || null structLitTypeArgs
                            then []
                            else zip (map (\(Parser.GenericExpr n _) -> n) structGenerics) structLitTypeArgs
                let structGenericNames = map (\(Parser.GenericExpr n _) -> n) structGenerics
                unless (null structLitTypeArgs) $ do
                    forM_ structLitTypeArgs $ \typeArg -> do
                        validateTypeParameters structGenericNames typeArg structLitPos ("type literal " ++ structLitName)
                    forM_ (zip structGenerics structLitTypeArgs) $ \(Parser.GenericExpr genName genConstraint, typeArg) -> do
                        case genConstraint of
                            Just constraint -> do
                                constraintName <- case constraint of
                                    Parser.StructT traitName [] -> return traitName
                                    _ -> do
                                        cerror "Trait constraint must be a type" structLitPos
                                        return ""
                                impls <- implsFor (typeToString typeArg)
                                unless (constraintName `elem` impls || typeToString typeArg == constraintName) $ do
                                    cerror ("Type argument " ++ show typeArg ++ " does not satisfy trait constraint " ++ constraintName ++ " for generic parameter " ++ genName) structLitPos
                            Nothing -> return ()
                let substitutedFields =
                        if null genericSubstitutions
                            then declaredFields
                            else map (second (`substituteGenerics` genericSubstitutions)) declaredFields
                let substitutedFieldMap = Map.fromList substitutedFields
                forM_ structLitFields $ \(fieldName, fieldValue) -> do
                    case Map.lookup fieldName substitutedFieldMap of
                        Just declaredType -> do
                            actualType <- typeOf fieldValue
                            unless (Parser.compareTypes actualType declaredType) $ do
                                cerror ("Type mismatch for field '" ++ fieldName ++ "': expected " ++ show declaredType ++ ", got " ++ show actualType) structLitPos
                        Nothing -> do
                            cerror ("Unknown field '" ++ fieldName ++ "' in type " ++ structLitName) structLitPos
                unless skipCheck $ do
                    case expr of
                        Parser.Struct{refinement, refinementSrc} -> do
                            case refinement of
                                Just rf -> do
                                    r <- runRefinement compileProgram' rf sl
                                    case r of
                                        Just False -> cerror ("Refinement failed (" ++ refinementSrc ++ ")") structLitPos
                                        Just True -> return ()
                                        Nothing -> cerror ("Cannot verify refinement (" ++ refinementSrc ++ ") at compile time - type fields contain variables without refinement guarantees") structLitPos
                                Nothing -> do
                                    implsForStruct <- implsFor structLitName
                                    traits' <- gets CS.traits
                                    forM_ implsForStruct $ \traitName -> do
                                        case find (\t -> Parser.name t == traitName) traits' of
                                            Just (Parser.Trait{refinement = traitRefinement, refinementSrc = traitRefinementSrc}) -> do
                                                case traitRefinement of
                                                    Just rf -> do
                                                        let structFields = case expr of
                                                                Parser.Struct{fields} -> fields
                                                                _ -> []
                                                        let transformedRefinement = transformRefinementExpr rf structFields
                                                        r <- runRefinement compileProgram' transformedRefinement sl
                                                        case r of
                                                            Just False -> cerror ("Trait '" ++ traitName ++ "' refinement failed (" ++ traitRefinementSrc ++ ") for type literal " ++ structLitName) structLitPos
                                                            Just True -> return ()
                                                            Nothing -> return ()
                                                    Nothing -> return ()
                                            _ -> return ()
                        _ -> return ()
            _ -> return ()
        Nothing -> return ()
    fields' <- mapM ((`compileExpr'` Parser.Any) . snd) structLitFields
    let names = map (DString . fst) structLitFields
    let instructions = zip names fields' >>= \(name', field) -> field ++ [Push name']
    implsForStruct <- implsFor structLitName
    return $ instructions ++ [Push $ DString structLitName, Push $ DString "__name", Push $ DList (map DString implsForStruct), Push $ DString "__traits", PackMap $ length structLitFields * 2 + 4]
compileStructLit _ _ expr _ = internalError $ "compileStructLit: unexpected " ++ show expr

compileStructAccess :: CompileExpr -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
compileStructAccess compileExpr' (Parser.StructAccess{structAccessStruct, structAccessField}) expectedType = do
    struct' <- compileExpr' structAccessStruct expectedType
    case structAccessField of
        Parser.Var{varName = field} -> return $ struct' ++ [Access field]
        _ -> do
            fieldExpr' <- compileExpr' structAccessField expectedType
            return $ struct' ++ fieldExpr' ++ [AAccess]
compileStructAccess _ expr _ = internalError $ "compileStructAccess: unexpected " ++ show expr

compileCast :: CompileExpr -> CompileProgram -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
compileCast compileExpr' compileProgram' (Parser.Cast{castExpr, castType, castPos}) expectedType = do
    structDecs' <- gets CS.structDecs
    let targetStructName = case castType of
            Parser.Var{varName} -> Just varName
            _ -> Nothing
    let targetStruct = case targetStructName of
            Just name -> find (\case Parser.Struct{name = name'} -> name' == name; _ -> False) structDecs'
            Nothing -> Nothing
    case targetStruct of
        Just (Parser.Struct{isValueStruct = True, fields, refinement, refinementSrc}) -> do
            when (length fields /= 1) $ cerror "value struct must have exactly one field" castPos
            let (fieldName, fieldType) = head fields
            fromType <- typeOf castExpr
            if not (Parser.compareTypes fromType fieldType)
                then do
                    cerror ("Type mismatch: cannot cast " ++ show fromType ++ " to value struct field type " ++ show fieldType) castPos
                    return []
                else do
                    let structLit = Parser.StructLit{structLitName = fromMaybe "" targetStructName, structLitFields = [(fieldName, castExpr)], structLitTypeArgs = [], structLitPos = castPos}
                    skipCheck <- gets CS.skipRefinementCheck
                    unless skipCheck $ do
                        case refinement of
                            Just rf -> do
                                r <- runRefinement compileProgram' rf structLit
                                case r of
                                    Just False -> cerror ("Refinement failed (" ++ refinementSrc ++ ")") castPos
                                    Just True -> return ()
                                    Nothing -> cerror ("Cannot verify refinement (" ++ refinementSrc ++ ") at compile time - value contains variables without refinement guarantees") castPos
                            Nothing -> return ()
                    modify (\s -> s{skipRefinementCheck = True})
                    result <- compileExpr' structLit expectedType
                    modify (\s -> s{skipRefinementCheck = skipCheck})
                    return result
        _ -> do
            from' <- compileExpr' castExpr Parser.Unknown
            let to' = compileType castType
            return $ from' ++ to' ++ [Cast]
  where
    compileType :: Parser.Expr -> [Instruction]
    compileType (Parser.Var{varName = "Int"}) = [Push $ DInt 0]
    compileType (Parser.Var{varName = "Float"}) = [Push $ DFloat 0.0]
    compileType (Parser.Var{varName = "Double"}) = [Push $ DDouble 0.0]
    compileType (Parser.Var{varName = "Bool"}) = [Push $ DBool False]
    compileType (Parser.Var{varName = "Char"}) = [Push $ DChar '\0']
    compileType (Parser.Var{varName = "String"}) = [Push $ DString ""]
    compileType (Parser.Var{varName = "CPtr"}) = [Push $ DCPtr $ ptrToWordPtr nullPtr]
    compileType (Parser.ListLit{listLitExprs = [x]}) = compileType x ++ [PackList 1]
    compileType (Parser.TupleLit{tupleLitExprs}) = concatMap compileType tupleLitExprs ++ [PackList $ length tupleLitExprs]
    compileType (Parser.Var{}) = [Push DNone]
    compileType x = internalError $ "Cannot cast to type " ++ show x
compileCast _ _ expr _ = internalError $ "compileCast: unexpected " ++ show expr
