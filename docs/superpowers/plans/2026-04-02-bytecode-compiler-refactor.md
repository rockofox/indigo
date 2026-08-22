# BytecodeCompiler Refactor Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Split `lib/BytecodeCompiler.hs` (2,096 lines) into a thin orchestrator plus 10 focused submodules by expression category, while cleaning up the worst code smells (unsafe builtin dispatch table, shadow/restore deduplication, binary op unification, `mangleExprForImport` simplification, context stack consolidation, and internal error marker).

**Architecture:** `BytecodeCompiler.hs` becomes a ~150-line orchestrator that defines `compileExpr` as a dispatch table and re-exports all types/functions for backward compatibility. Each submodule receives `compileExpr` as a `CompileExpr` callback to avoid circular imports. `Compiler.State` is the leaf module with no project imports; everything else builds on top of it.

**Tech Stack:** Haskell, GHC 9.14, `mtl` StateT, `containers` Map, `fourmolu` formatter, `hlint`, `cabal`.

---

## Files

| Action | Path | Responsibility |
|--------|------|----------------|
| Create | `lib/Compiler/State.hs` | `CompilerState`, `Function`, `Let`, `CompilerError`, `External`, `CompileExpr` type alias, `Compiler` type alias, `cerror`, `internalError`, `allocId`, `getErrorCount`, all `initCompilerState*` functions, `implsFor`, helper utilities |
| Create | `lib/Compiler/Types.hs` | `typeOf`, `safeTypeOf`, `typeCompatible`, `compareTypes'`, `functionTypesAcceptable`, `typeToString`, `allTheSame`, `evaluateFunction`, `compileTypeFromType`, `typeToData`, `structNameFromType` |
| Create | `lib/Compiler/Literals.hs` | `compileLiterals` — handles `IntLit`, `FloatLit`, `DoubleLit`, `StringLit`, `BoolLit`, `CharLit`, `UnaryMinus` on literals |
| Create | `lib/Compiler/Functions.hs` | `compileFuncCall`, `compileFuncDef`, `compileFuncDec`, `compileFunction`, `compileLambda`, `compileVar`, `compileLet`, `compileParenApply`, `unsafeBuiltins` map, `compileBinOp` (unified), `getBinOpPosition`, `extractPosition`, `findFunction`, `findAnyFunction`, `typesMatch`, `typesMatchExactly`, `unmangleFunctionName`, `internalFunctions`, `compileParameter` |
| Create | `lib/Compiler/Structs.hs` | `compileStruct`, `compileStructLit`, `compileStructAccess`, `compileTupleLit`, `compileTupleAccess`, `compileCast`, `validateTypeParameters`, `getStructFields`, `compileTypeInstr` |
| Create | `lib/Compiler/Traits.hs` | `compileTrait`, `compileImpl`, `createVirtualFunctions`, `substituteGenerics` |
| Create | `lib/Compiler/ControlFlow.hs` | `compileIf`, `compileWhen`, `compileDoBlock`, `compilePipeline`, `compileThen`, `compileFlexible`, `compileStrictEval`, `withShadowedVars`, `extractPatternVars`, `compilePatternMatch` |
| Create | `lib/Compiler/Lists.hs` | `compileListLit`, `compileListAdd`, `compileListConcat`, `compileArrayAccess` |
| Create | `lib/Compiler/Modules.hs` | `compileImport`, `mangleExprForImport`, `buildModuleMap`, `findSourceFile`, `findSourceFile'`, `findFirstModuleFile`, `preludeFile`, `extractModuleName`, `extractDefinedFuncNames`, `extractDefinedStructNames` |
| Create | `lib/Compiler/Refinements.hs` | `transformRefinementExpr`, `runRefinement`, `extractVariables`, `extractLets`, `extractFunctions` |
| Modify | `lib/BytecodeCompiler.hs` | Thin orchestrator: imports submodules, defines `compileExpr` dispatch table, re-exports everything |
| Modify | `indigo.cabal` | Add `Compiler.*` modules to `other-modules` |

---

## Task 1: Create `Compiler.State`

**Files:**
- Create: `lib/Compiler/State.hs`

This is the leaf module. Everything else depends on it. Move all data types, type aliases, and stateful utilities here.

- [ ] **Step 1: Create `lib/Compiler/State.hs`**

```haskell
{-# LANGUAGE CPP #-}

module Compiler.State where

import AST (Position (..), anyPosition, zeroPosition)
import AST qualified as Parser
import Control.Monad.State (StateT, gets, modify)
import Data.Map qualified as Map
import VM (Instruction)

-- | Type alias for all compiler monadic actions
type Compiler a = StateT (CompilerState IO) IO a

-- | Callback type passed to submodules to recurse into compileExpr
type CompileExpr = Parser.Expr -> Parser.Type -> Compiler [Instruction]

data Function = Function
    { baseName :: String
    , funame :: String
    , function :: [Instruction]
    , types :: [Parser.Type]
    , context :: String
    }
    deriving (Show)

data External = External
    { name :: String
    , returnType :: Parser.Type
    , args :: [Parser.Type]
    , from :: String
    }
    deriving (Show)

data CompilerError = CompilerError
    { errorMessage :: String
    , errorPosition :: AST.Position
    , errorFile :: String
    }
    deriving (Show, Eq)

data Let = Let
    { name :: String
    , vtype :: Parser.Type
    , context :: String
    }
    deriving (Show)

-- | contextStack replaces the old currentContext + contextPath pair.
-- The head is the current context; the list is the full path from outermost to current.
-- Invariant: never empty (initialized to ["__outside"]).
data CompilerState a = CompilerState
    { program :: Parser.Program
    , functions :: [Function]
    , funcDefs :: [Parser.Expr]
    , funcDecs :: [Parser.Expr]
    , structDecs :: [Parser.Expr]
    , lastLabel :: Int
    , lets :: [Let]
    , traits :: [Parser.Expr]
    , impls :: [Parser.Expr]
    , contextStack :: [String]   -- head = currentContext; non-empty invariant
    , externals :: [External]
    , functionsByTrait :: [(String, String, String, Parser.Expr)]
    , errors :: [CompilerError]
    , sourcePath :: String
    , skipRefinementCheck :: Bool
    , modules :: Map.Map String (Parser.Program, FilePath)
    , currentModule :: Maybe String
    , currentSourceFile :: String
    }

-- | Smart accessor: current innermost context
currentContext :: CompilerState a -> String
currentContext s = case contextStack s of
    [] -> "__outside"
    (c : _) -> c

-- | Smart accessor: the old contextPath field (full path list)
contextPath :: CompilerState a -> [String]
contextPath = contextStack

initCompilerState :: Parser.Program -> CompilerState a
initCompilerState prog =
    CompilerState
        { program = prog
        , functions = []
        , funcDefs = []
        , funcDecs = []
        , structDecs = []
        , lastLabel = 0
        , lets = []
        , traits = []
        , impls = []
        , contextStack = ["__outside"]
        , externals = []
        , functionsByTrait = []
        , errors = []
        , sourcePath = "no file"
        , skipRefinementCheck = False
        , modules = Map.empty
        , currentModule = Nothing
        , currentSourceFile = "no file"
        }

initCompilerStateWithFile :: Parser.Program -> String -> CompilerState a
initCompilerStateWithFile prog sourcePath' =
    (initCompilerState prog)
        { sourcePath = sourcePath'
        , currentSourceFile = sourcePath'
        }

initCompilerStateWithModules :: Map.Map String (Parser.Program, FilePath) -> Parser.Program -> String -> CompilerState a
initCompilerStateWithModules modules' prog sourcePath' =
    (initCompilerStateWithFile prog sourcePath')
        { modules = modules'
        , currentModule = Parser.moduleName prog
        }

cerror :: String -> AST.Position -> Compiler ()
cerror msg pos = do
    currentFile <- gets currentSourceFile
    case pos of
        AST.Position (p, _) | p < 0 -> return ()
        _ | pos == AST.zeroPosition -> return ()
        _ -> modify (\s -> s{errors = errors s ++ [CompilerError msg pos currentFile]})

-- | Mark a compiler bug (impossible state). Crashes with a [BUG] prefix.
internalError :: String -> a
internalError msg = errorWithoutStackTrace ("[BUG] " ++ msg)

allocId :: Compiler Int
allocId = do
    s <- gets lastLabel
    modify (\s' -> s'{lastLabel = s + 1})
    return s

getErrorCount :: Compiler Int
getErrorCount = gets (length . errors)

implsFor :: String -> Compiler [String]
implsFor structName = do
    impls' <- gets impls
    return
        [ Parser.trait impl
        | impl <- impls'
        , let forName = case Parser.for impl of
                Parser.StructT n _ -> n
                _ -> ""
        , forName == structName
        ]
```

- [ ] **Step 2: Build to check it compiles**

```bash
cabal build indigo 2>&1 | head -40
```

Expected: errors about missing modules (we haven't wired it yet) but `Compiler.State` itself should be error-free if listed in cabal. Skip this step and proceed to Task 2 — we wire cabal in Task 11.

---

## Task 2: Create `Compiler.Types`

**Files:**
- Create: `lib/Compiler/Types.hs`

Move type inference and type checking functions here. They depend only on `Compiler.State`.

- [ ] **Step 1: Create `lib/Compiler/Types.hs`**

```haskell
module Compiler.Types where

import AST qualified as Parser
import Compiler.State
import Control.Monad.State (gets)
import Data.List (find)
import Data.Map qualified as Map
import Foreign (nullPtr, ptrToWordPtr)
import VM

typeToString :: Parser.Type -> String
typeToString (Parser.StructT name []) = name
typeToString (Parser.StructT name args) = name ++ "<" ++ concatMap (\t -> typeToString t ++ ",") args ++ ">"
typeToString (Parser.List t) = "[" ++ typeToString t ++ "]"
typeToString (Parser.Tuple ts) = "(" ++ concatMap (\t -> typeToString t ++ ",") ts ++ ")"
typeToString (Parser.Fn args ret) = "(" ++ concatMap (\t -> typeToString t ++ " -> ") args ++ typeToString ret ++ ")"
typeToString Parser.Any = "Any"
typeToString Parser.Unknown = "Unknown"
typeToString Parser.None = "None"
typeToString Parser.Self = "Self"

structNameFromType :: Parser.Type -> String
structNameFromType (Parser.StructT name _) = name
structNameFromType _ = ""

evaluateFunction :: [Parser.Type] -> Int -> Parser.Type
evaluateFunction types taken = case drop taken types of
    [] -> Parser.Unknown
    [x] -> x
    xs -> Parser.Fn xs (last xs)

typeToData :: Parser.Type -> Data
typeToData (Parser.StructT "Int" []) = DInt 0
typeToData (Parser.StructT "Float" []) = DFloat 0.0
typeToData (Parser.StructT "Double" []) = DDouble 0.0
typeToData (Parser.StructT "Bool" []) = DBool False
typeToData (Parser.StructT "Char" []) = DChar '\0'
typeToData (Parser.StructT "String" []) = DString ""
typeToData (Parser.StructT "CPtr" []) = DCPtr $ ptrToWordPtr nullPtr
typeToData (Parser.List _) = DList []
typeToData _ = DNone

compileTypeFromType :: Parser.Type -> [Instruction]
compileTypeFromType t = [Push (typeToData t)]

allTheSame :: [Parser.Type] -> Bool
allTheSame [] = True
allTheSame [_] = True
allTheSame xs = all (compareSame (head xs)) (tail xs)
  where
    compareSame (Parser.List x) (Parser.List y) = compareSame x y
    compareSame x y = x == y

typeCompatible :: Parser.Type -> Parser.Type -> Compiler Bool
typeCompatible x y = do
    let x' = typeToString x
    let y' = typeToString y
    implsX <- implsFor x'
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
                else return $ y' `elem` implsX

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

functionTypesAcceptable :: [Parser.Type] -> [Parser.Type] -> [Parser.GenericExpr] -> Compiler Bool
functionTypesAcceptable use def generics = do
    let genericNames = map (\(Parser.GenericExpr name _) -> name) generics
    let useAndDef = zip use def
    let udStructs = concatMap (\(a, b) -> case (a, b) of (_, Parser.StructT _ _) -> [(a, b)]; (Parser.List c'@(Parser.StructT _ _), Parser.List c@(Parser.StructT _ _)) -> [(c', c)]; _ -> []) useAndDef
    let udStructsGeneric = filter (\case (_, Parser.StructT b _) -> b `elem` genericNames; (_, Parser.List (Parser.StructT b _)) -> b `elem` genericNames; _ -> internalError "Impossible in functionTypesAcceptable") udStructs
    let groupedUdStructsGeneric = map (\x -> (x, fst <$> filter (\case (_, Parser.StructT b _) -> b == x; (_, Parser.List (Parser.StructT b _)) -> b == x; _ -> internalError "Impossible in functionTypesAcceptable") udStructsGeneric)) genericNames
    let genericsMatch = all (\(_, types) -> allTheSame types) groupedUdStructsGeneric
    typesMatch' <- allM (uncurry $ uncurry compareTypes') $ zip (zip use def) [generics]
    return $ typesMatch' && genericsMatch

allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM _ [] = return True
allM f (x : xs) = f x >>= \b -> if b then allM f xs else return False

typeOf :: Parser.Expr -> Compiler Parser.Type
typeOf Parser.FuncCall{funcName, funcArgs} = do
    funcDecs' <- gets funcDecs
    let a =
            maybe
                [Parser.Unknown]
                Parser.types
                (find (\x -> x.name == funcName) funcDecs')
    return $ evaluateFunction a (length funcArgs)
typeOf Parser.Var{varName} = do
    lets' <- gets lets
    return $ maybe Parser.Unknown vtype (find (\x -> x.name == varName) lets')
typeOf (Parser.IntLit _ _) = return $ Parser.StructT "Int" []
typeOf (Parser.FloatLit _ _) = return $ Parser.StructT "Float" []
typeOf (Parser.BoolLit _ _) = return $ Parser.StructT "Bool" []
typeOf (Parser.StringLit _ _) = return $ Parser.StructT "String" []
typeOf (Parser.Add{addLhs = x}) = typeOf x
typeOf (Parser.Sub{subLhs = x}) = typeOf x
typeOf (Parser.Mul{mulLhs = x}) = typeOf x
typeOf (Parser.Div{divLhs = x}) = typeOf x
typeOf (Parser.Power{powerBase = x}) = typeOf x
typeOf (Parser.UnaryMinus{unaryMinusExpr = x}) = typeOf x
typeOf (Parser.Eq{}) = return $ Parser.StructT "Bool" []
typeOf (Parser.Neq{}) = return $ Parser.StructT "Bool" []
typeOf (Parser.Lt{}) = return $ Parser.StructT "Bool" []
typeOf (Parser.Gt{}) = return $ Parser.StructT "Bool" []
typeOf (Parser.Le{}) = return $ Parser.StructT "Bool" []
typeOf (Parser.Ge{}) = return $ Parser.StructT "Bool" []
typeOf (Parser.And{}) = return $ Parser.StructT "Bool" []
typeOf (Parser.Or{}) = return $ Parser.StructT "Bool" []
typeOf (Parser.Not{}) = return $ Parser.StructT "Bool" []
typeOf (Parser.Let{}) = internalError "typeOf: cannot infer type of let"
typeOf (Parser.If _ b _ _) = typeOf b
typeOf (Parser.FuncDef{}) = internalError "typeOf: cannot infer type of FuncDef"
typeOf (Parser.FuncDec{}) = internalError "typeOf: cannot infer type of FuncDec"
typeOf (Parser.Function{}) = return Parser.Unknown
typeOf (Parser.ExternDec{}) = internalError "typeOf: cannot infer type of ExternDec"
typeOf (Parser.Discard{}) = internalError "typeOf: cannot infer type of Discard"
typeOf (Parser.Import{}) = internalError "typeOf: cannot infer type of Import"
typeOf (Parser.Ref{}) = internalError "typeOf: cannot infer type of Ref"
typeOf (Parser.Struct{}) = internalError "typeOf: cannot infer type of Struct"
typeOf (Parser.StructLit{structLitName, structLitTypeArgs}) = return $ Parser.StructT structLitName structLitTypeArgs
typeOf (Parser.ListLit{listLitExprs}) = case listLitExprs of
    [] -> return $ Parser.List Parser.Any
    (x : _) -> typeOf x >>= \t -> return $ Parser.List t
typeOf (Parser.ListConcat{listConcatLhs = x}) = typeOf x
typeOf (Parser.StructAccess{structAccessStruct = x}) = typeOf x
typeOf (Parser.Pipeline{pipelineRhs = x}) = typeOf x
typeOf (Parser.Lambda{lambdaArgs, lambdaBody}) = do
    returnType <- typeOf lambdaBody
    return $ Parser.Fn (replicate (length lambdaArgs) Parser.Any) returnType
typeOf (Parser.Cast{castType}) = case castType of
    Parser.Var{varName} -> return $ Parser.StructT varName []
    _ -> return Parser.Unknown
typeOf (Parser.TypeLit{}) = return Parser.Unknown
typeOf (Parser.Flexible{flexibleExpr = x}) = typeOf x
typeOf (Parser.Trait{}) = internalError "typeOf: cannot infer type of Trait"
typeOf (Parser.Impl{}) = internalError "typeOf: cannot infer type of Impl"
typeOf (Parser.Then{thenRhs = x}) = typeOf x
typeOf (Parser.StrictEval{strictEvalExpr = x}) = typeOf x
typeOf (Parser.External{}) = return Parser.Unknown
typeOf (Parser.When _ branches else_ _) = case branches of
    [] -> maybe (return Parser.Unknown) typeOf else_
    ((_, body) : _) -> typeOf body
typeOf (Parser.DoBlock exprs _) = if null exprs then return Parser.None else typeOf (last exprs)
typeOf (Parser.ParenApply{parenApplyExpr = x}) = typeOf x
typeOf (Parser.ListAdd{listAddLhs = x}) = typeOf x
typeOf (Parser.CharLit _ _) = return $ Parser.StructT "Char" []
typeOf (Parser.DoubleLit _ _) = return $ Parser.StructT "Double" []
typeOf (Parser.TupleLit{tupleLitExprs}) = do
    ts <- mapM typeOf tupleLitExprs
    return $ Parser.Tuple ts
typeOf (Parser.TupleAccess{tupleAccessTuple = x, tupleAccessIndex = i}) = do
    t <- typeOf x
    case t of
        Parser.Tuple ts | i < length ts -> return (ts !! i)
        _ -> return Parser.Unknown
typeOf _ = return Parser.Unknown

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
safeTypeOf (Parser.DoBlock exprs _) = if null exprs then return Parser.None else safeTypeOf (last exprs)
safeTypeOf (Parser.If _ b _ _) = safeTypeOf b
safeTypeOf (Parser.When _ branches else_ _) = case branches of
    [] -> maybe (return Parser.Unknown) safeTypeOf else_
    ((_, body) : _) -> safeTypeOf body
safeTypeOf expr = typeOf expr

getStructFields :: String -> Compiler [(String, Parser.Type)]
getStructFields structName = do
    structDecs' <- gets structDecs
    case find (\case Parser.Struct{name = n} -> n == structName; _ -> False) structDecs' of
        Just (Parser.Struct{fields}) -> return fields
        _ -> return []
```

- [ ] **Step 2: Note — `allM` from `Control.Monad.Loops` is currently used; the version above defines it locally instead. Verify the original import and remove the `monad-loops` dependency from `Types.hs` if applicable. For now move on.**

---

## Task 3: Create `Compiler.Refinements`

**Files:**
- Create: `lib/Compiler/Refinements.hs`

Move `transformRefinementExpr`, `runRefinement`, `extractVariables`, `extractLets`, `extractFunctions`. These are pure or use `compileProgram` via callback.

- [ ] **Step 1: Create `lib/Compiler/Refinements.hs`**

```haskell
module Compiler.Refinements where

import AST qualified as Parser
import Compiler.State
import Control.Monad.State (StateT, evalStateT, get, gets, modify)
import Data.List (find, nub)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe, maybeToList)
import VM

-- | Transform a refinement expression so field name references become struct accesses.
transformRefinementExpr :: Parser.Expr -> [(String, Parser.Type)] -> Parser.Expr
transformRefinementExpr expr fieldNames = case expr of
    Parser.Var{varName}
        | varName `elem` map fst fieldNames ->
            Parser.StructAccess
                (Parser.Var "it" Parser.zeroPosition)
                (Parser.Var varName Parser.zeroPosition)
                Parser.anyPosition
    Parser.Add{addLhs = x, addRhs = y, addPos} ->
        Parser.Add (transformRefinementExpr x fieldNames) (transformRefinementExpr y fieldNames) addPos
    Parser.Sub{subLhs = x, subRhs = y, subPos} ->
        Parser.Sub (transformRefinementExpr x fieldNames) (transformRefinementExpr y fieldNames) subPos
    Parser.Mul{mulLhs = x, mulRhs = y, mulPos} ->
        Parser.Mul (transformRefinementExpr x fieldNames) (transformRefinementExpr y fieldNames) mulPos
    Parser.Div{divLhs = x, divRhs = y, divPos} ->
        Parser.Div (transformRefinementExpr x fieldNames) (transformRefinementExpr y fieldNames) divPos
    Parser.Modulo{moduloLhs = x, moduloRhs = y, moduloPos} ->
        Parser.Modulo (transformRefinementExpr x fieldNames) (transformRefinementExpr y fieldNames) moduloPos
    Parser.Gt{gtLhs = x, gtRhs = y, gtPos} ->
        Parser.Gt (transformRefinementExpr x fieldNames) (transformRefinementExpr y fieldNames) gtPos
    Parser.Lt{ltLhs = x, ltRhs = y, ltPos} ->
        Parser.Lt (transformRefinementExpr x fieldNames) (transformRefinementExpr y fieldNames) ltPos
    Parser.Ge{geLhs = x, geRhs = y, gePos} ->
        Parser.Ge (transformRefinementExpr x fieldNames) (transformRefinementExpr y fieldNames) gePos
    Parser.Le{leLhs = x, leRhs = y, lePos} ->
        Parser.Le (transformRefinementExpr x fieldNames) (transformRefinementExpr y fieldNames) lePos
    Parser.Eq{eqLhs = x, eqRhs = y, eqPos} ->
        Parser.Eq (transformRefinementExpr x fieldNames) (transformRefinementExpr y fieldNames) eqPos
    Parser.Neq{neqLhs = x, neqRhs = y, neqPos} ->
        Parser.Neq (transformRefinementExpr x fieldNames) (transformRefinementExpr y fieldNames) neqPos
    Parser.And{andLhs = x, andRhs = y, andPos} ->
        Parser.And (transformRefinementExpr x fieldNames) (transformRefinementExpr y fieldNames) andPos
    Parser.Or{orLhs = x, orRhs = y, orPos} ->
        Parser.Or (transformRefinementExpr x fieldNames) (transformRefinementExpr y fieldNames) orPos
    Parser.Not{notExpr = x, notPos} ->
        Parser.Not (transformRefinementExpr x fieldNames) notPos
    Parser.UnaryMinus{unaryMinusExpr = x, unaryMinusPos} ->
        Parser.UnaryMinus (transformRefinementExpr x fieldNames) unaryMinusPos
    Parser.StructAccess{structAccessStruct = s, structAccessField = f, structAccessPos} ->
        Parser.StructAccess (transformRefinementExpr s fieldNames) f structAccessPos
    Parser.FuncCall{funcName, funcArgs, funcPos} ->
        Parser.FuncCall funcName (map (`transformRefinementExpr` fieldNames) funcArgs) funcPos
    _ -> expr

extractVariables :: Parser.Expr -> [String]
extractVariables expr = case expr of
    Parser.Var{varName} -> [varName]
    Parser.StructLit{structLitFields} -> concatMap (extractVariables . snd) structLitFields
    Parser.FuncCall{funcArgs} -> concatMap extractVariables funcArgs
    Parser.Add{addLhs, addRhs} -> extractVariables addLhs ++ extractVariables addRhs
    Parser.Sub{subLhs, subRhs} -> extractVariables subLhs ++ extractVariables subRhs
    Parser.Mul{mulLhs, mulRhs} -> extractVariables mulLhs ++ extractVariables mulRhs
    Parser.Div{divLhs, divRhs} -> extractVariables divLhs ++ extractVariables divRhs
    Parser.Gt{gtLhs, gtRhs} -> extractVariables gtLhs ++ extractVariables gtRhs
    Parser.Lt{ltLhs, ltRhs} -> extractVariables ltLhs ++ extractVariables ltRhs
    Parser.Ge{geLhs, geRhs} -> extractVariables geLhs ++ extractVariables geRhs
    Parser.Le{leLhs, leRhs} -> extractVariables leLhs ++ extractVariables leRhs
    Parser.Eq{eqLhs, eqRhs} -> extractVariables eqLhs ++ extractVariables eqRhs
    Parser.Neq{neqLhs, neqRhs} -> extractVariables neqLhs ++ extractVariables neqRhs
    Parser.And{andLhs, andRhs} -> extractVariables andLhs ++ extractVariables andRhs
    Parser.Or{orLhs, orRhs} -> extractVariables orLhs ++ extractVariables orRhs
    Parser.Not{notExpr} -> extractVariables notExpr
    Parser.UnaryMinus{unaryMinusExpr} -> extractVariables unaryMinusExpr
    Parser.StructAccess{structAccessStruct, structAccessField} -> extractVariables structAccessStruct ++ extractVariables structAccessField
    Parser.If{ifCond, ifThen, ifElse} -> extractVariables ifCond ++ extractVariables ifThen ++ extractVariables ifElse
    Parser.Let{letValue} -> extractVariables letValue
    Parser.DoBlock{doBlockExprs} -> concatMap extractVariables doBlockExprs
    Parser.ListLit{listLitExprs} -> concatMap extractVariables listLitExprs
    Parser.ParenApply{parenApplyExpr, parenApplyArgs} -> extractVariables parenApplyExpr ++ concatMap extractVariables parenApplyArgs
    _ -> []

extractLets :: [Parser.Expr] -> [Parser.Expr]
extractLets = concatMap extractLet
  where
    extractLet expr@(Parser.Let{letValue}) = expr : extractLets [letValue]
    extractLet (Parser.FuncDef{body}) = extractLets [body]
    extractLet (Parser.DoBlock{doBlockExprs}) = extractLets doBlockExprs
    extractLet (Parser.If{ifThen, ifElse}) = extractLets [ifThen, ifElse]
    extractLet (Parser.Function{def}) = extractLets def
    extractLet _ = []

extractFunctions :: [Parser.Expr] -> [Parser.Expr]
extractFunctions = concatMap extractFunction
  where
    extractFunction expr@(Parser.Function{}) = [expr]
    extractFunction expr@(Parser.FuncDef{body}) = expr : extractFunctions [body]
    extractFunction expr@(Parser.FuncDec{}) = [expr]
    extractFunction (Parser.Let{letName, letValue}) = case letValue of
        Parser.Function{def, dec} ->
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
             in [Parser.Function{def = renamedDef, dec = renamedDec, functionPos = Parser.anyPosition}]
        _ -> extractFunctions [letValue]
    extractFunction (Parser.DoBlock{doBlockExprs}) = extractFunctions doBlockExprs
    extractFunction _ = []

-- | Run a refinement predicate against a value at compile time.
-- Returns Nothing if it cannot be verified (variables with no compile-time value).
-- Takes compileProgram as a callback to avoid circular imports.
runRefinement
    :: (Parser.Program -> StateT (CompilerState IO) IO (Either [Instruction] [CompilerError]))
    -> Parser.Expr
    -> Parser.Expr
    -> Compiler (Maybe Bool)
runRefinement compileProgram' refinement value = do
    currentState <- get
    let structDef = case value of
            Parser.StructLit{structLitName} ->
                find (\case Parser.Struct{name = name'} -> name' == structLitName; _ -> False) currentState.structDecs
            _ -> Nothing
    let fieldNames = case structDef of
            Just (Parser.Struct{fields}) -> fields
            _ -> []
    let transformedRefinement = if null fieldNames then refinement else transformRefinementExpr refinement fieldNames
    let currentProgramExprs = Parser.exprs currentState.program
    let allFunctionDefs = extractFunctions currentProgramExprs
    let functionDefs = filter (\case Parser.FuncDec{name} -> name /= "main"; Parser.FuncDef{name} -> name /= "main"; Parser.Function{dec = Parser.FuncDec{name}} -> name /= "main"; _ -> True) allFunctionDefs
    let referencedVars = nub $ extractVariables value
    let allLets = extractLets currentProgramExprs
    let neededLets = filter (\case Parser.Let{letName} -> letName `elem` referencedVars; _ -> False) allLets
    let letNames = map (\case Parser.Let{letName} -> letName; _ -> "") neededLets
    let unresolvedVars = filter (`notElem` letNames) referencedVars
    if not (null unresolvedVars)
        then return Nothing
        else do
            let mainBody =
                    if null neededLets
                        then [Parser.FuncCall "__refinement" [value] Parser.anyPosition]
                        else neededLets ++ [Parser.FuncCall "__refinement" [value] Parser.anyPosition]
            let structDefWithoutIs = case structDef of
                    Just (Parser.Struct{name, fields, refinement = r, refinementSrc, isValueStruct, generics, structPos}) ->
                        Just (Parser.Struct{name = name, fields = fields, refinement = r, refinementSrc = refinementSrc, is = [], isValueStruct = isValueStruct, generics = generics, structPos = structPos})
                    _ -> structDef
            let progExprs =
                    maybeToList structDefWithoutIs
                        ++ functionDefs
                        ++ [ Parser.FuncDec "__refinement" [Parser.Any, Parser.StructT "Bool" []] [] Parser.anyPosition
                           , Parser.FuncDef "__refinement" [Parser.Var "it" Parser.zeroPosition] transformedRefinement Parser.anyPosition
                           , Parser.FuncDec "main" [Parser.StructT "IO" []] [] Parser.anyPosition
                           , Parser.FuncDef
                                "main"
                                []
                                (Parser.DoBlock mainBody Parser.anyPosition)
                                Parser.anyPosition
                           ]
            let prog = Parser.Program progExprs Nothing
            let freshState = initCompilerState prog
                extractedFuncDecs = filter (\case Parser.FuncDec{} -> True; _ -> False) functionDefs
                stateWithContext =
                    freshState
                        { funcDecs = currentState.funcDecs ++ extractedFuncDecs ++ [Parser.FuncDec "__refinement" [Parser.Any, Parser.StructT "Bool" []] [] Parser.anyPosition]
                        , structDecs = currentState.structDecs
                        , skipRefinementCheck = True
                        }
            compiled <- liftIO $ evalStateT (compileProgram' prog) stateWithContext
            case compiled of
                Right _ -> return $ Just False
                Left bytecode -> do
                    let mainPc = locateLabel bytecode "main"
                    let vm = (initVM (V.fromList bytecode)){pc = mainPc, callStack = [StackFrame{returnAddress = 0, locals = []}, StackFrame{returnAddress = mainPc, locals = []}], shouldExit = False, running = True}
                    result <- liftIO $ runVMVM vm
                    let finalStack = stack result
                    let isRunning = running result
                    if isRunning
                        then return $ Just False
                        else return $ Just $ case find (\case DBool _ -> True; _ -> False) finalStack of
                            Just (DBool b) -> b
                            _ -> False
```

- [ ] **Step 2: Note — `runRefinement` needs `liftIO`, `V.fromList`, `locateLabel`, `initVM`, `runVMVM`, `StackFrame`. Add the appropriate imports: `Control.Monad.State (liftIO)`, `Data.Vector qualified as V`, `VM (locateLabel, initVM, runVMVM, StackFrame, stack, running, shouldExit)`. Wire these up when completing the module.**

---

## Task 4: Create `Compiler.Lists`

**Files:**
- Create: `lib/Compiler/Lists.hs`

- [ ] **Step 1: Create `lib/Compiler/Lists.hs`**

```haskell
module Compiler.Lists where

import AST qualified as Parser
import Compiler.State
import VM

compileLists :: CompileExpr -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
compileLists compileExpr expr expectedType = case expr of
    Parser.ListLit{listLitExprs} -> do
        elems <- concatMapM (`compileExpr` expectedType) listLitExprs
        return $ elems ++ [PackList $ length listLitExprs]
    Parser.TupleLit{tupleLitExprs} -> do
        elems <- concatMapM (`compileExpr` expectedType) tupleLitExprs
        return $ elems ++ [PackList $ length tupleLitExprs]
    Parser.TupleAccess{tupleAccessTuple, tupleAccessIndex} -> do
        tuple' <- compileExpr tupleAccessTuple expectedType
        return $ tuple' ++ [Push $ DInt tupleAccessIndex, Index]
    Parser.ListConcat{listConcatLhs = a, listConcatRhs = b} -> do
        a' <- compileExpr a expectedType
        b' <- compileExpr b expectedType
        let a'' = case a' of
                [Meta "flex", a'''] -> [a'''] ++ b' ++ [Cast]
                _ -> a'
        let b'' = case b' of
                [Meta "flex", b'''] -> [b'''] ++ a' ++ [Cast]
                _ -> b'
        return (b'' ++ a'' ++ [Concat 2])
    Parser.ListAdd{listAddLhs = a, listAddRhs = b} -> do
        a' <- compileExpr a expectedType
        b' <- compileExpr b expectedType
        let a'' = case a' of
                [Meta "flex", a'''] -> [a'''] ++ b' ++ [Cast]
                _ -> a'
        let b'' = case b' of
                [Meta "flex", b'''] -> [b'''] ++ a' ++ [Cast]
                _ -> b'
        return (b'' ++ a'' ++ [ListAdd 2])
    _ -> return []

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = fmap concat (mapM f xs)
```

---

## Task 5: Create `Compiler.ControlFlow`

**Files:**
- Create: `lib/Compiler/ControlFlow.hs`

Move `compileIf`, `compileWhen`, `compileDoBlock`, `compilePipeline`, `compileThen`, `compileFlexible`, `compileStrictEval`, `withShadowedVars`, `extractPatternVars`, `compilePatternMatch`.

- [ ] **Step 1: Create `lib/Compiler/ControlFlow.hs`**

```haskell
module Compiler.ControlFlow where

import AST qualified as Parser
import Compiler.State
import Compiler.Types (typeOf)
import Control.Monad (forM_)
import Control.Monad.State (gets, modify)
import Data.List (groupBy, nub)
import VM

-- | Save variables that will be shadowed, run body, restore them.
withShadowedVars :: [String] -> Compiler [Instruction] -> Compiler [Instruction]
withShadowedVars varsToSave body = do
    saveInstrs <- mapM (\v -> return [LLoad v, LStore ("__shadow_" ++ v)]) varsToSave
    bodyInstrs <- body
    let restoreInstrs = concatMap (\v -> [LLoad ("__shadow_" ++ v), LStore v]) varsToSave
    return $ concat saveInstrs ++ bodyInstrs ++ restoreInstrs

compileIf :: CompileExpr -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
compileIf compileExpr (Parser.If{ifCond, ifThen, ifElse}) expectedType = do
    cond' <- compileExpr ifCond expectedType
    then' <- compileExpr ifThen expectedType
    else' <- compileExpr ifElse expectedType
    elseLabel <- allocId >>= \x -> return $ "else" ++ show x
    endLabel <- allocId >>= \x -> return $ "end" ++ show x
    return $ cond' ++ [Jf elseLabel] ++ then' ++ [Jmp endLabel, Label elseLabel] ++ else' ++ [Label endLabel]
compileIf _ expr _ = internalError $ "compileIf: unexpected expr: " ++ show expr

compileWhen :: CompileExpr -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
compileWhen compileExpr (Parser.When{whenExpr = expr, whenBranches = branches, whenElse = else_}) expectedType = do
    expr' <- compileExpr expr expectedType
    endLabel <- allocId >>= \x -> return $ "when_end" ++ show x
    branchLabels <- mapM (\_ -> allocId >>= \x -> return $ "when_next" ++ show x) branches
    let allPatternVars = nub $ concatMap (extractPatternVars . fst) branches
    lets' <- gets lets
    let existingVars = map (\l -> l.name) lets'
    let varsToSave = filter (`elem` existingVars) allPatternVars
    saveInstrs <- mapM (\v -> return [LLoad v, LStore ("__shadow_" ++ v)]) varsToSave
    let branchPairs = zip branches branchLabels
    branchInstrs <- mapM
        (\((pat, body), nextLabel) -> do
            patternInstrs <- compilePatternMatch compileExpr pat nextLabel expectedType
            bodyInstrs <- compileExpr body expectedType
            let restoreInstrs = concatMap (\v -> [LLoad ("__shadow_" ++ v), LStore v]) varsToSave
            return (patternInstrs, bodyInstrs, restoreInstrs))
        branchPairs
    elseInstrs <- case else_ of
        Just elseExpr -> compileExpr elseExpr expectedType
        Nothing -> return []
    let branchCode =
            concat $
                zipWith
                    (\(patternInstrs, bodyInstrs, restoreInstrs) nextLabel ->
                        patternInstrs ++ bodyInstrs ++ restoreInstrs ++ [Jmp endLabel, Label nextLabel])
                    branchInstrs
                    branchLabels
    return $ expr' ++ concat saveInstrs ++ branchCode ++ elseInstrs ++ [Label endLabel]
compileWhen _ expr _ = internalError $ "compileWhen: unexpected expr: " ++ show expr

compileDoBlock :: CompileExpr -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
compileDoBlock compileExpr (Parser.DoBlock{doBlockExprs = exprs}) expectedType = do
    letsBefore <- gets lets
    curCon <- gets currentContext
    let inScopeLets = filter (\l -> l.context == curCon || l.context == "__outside") letsBefore
    let existingVars = map (\l -> l.name) inScopeLets
    let shadowedVars =
            filter (`elem` existingVars) $
                map (\case Parser.Let{letName} -> letName; _ -> "") $
                    filter (\case Parser.Let{} -> True; _ -> False) exprs
    let grouped = groupBy (\a b -> isFuncCall a && isFuncCall b) exprs
    let nestedSequence = concatMap
            (\case
                [] -> []
                [x] ->
                    if isFuncCall x
                        then [Parser.FuncCall{funcName = "sequence", funcArgs = [x, Parser.FuncCall{funcName = "nop", funcArgs = [], funcPos = Parser.anyPosition}], funcPos = Parser.anyPosition}]
                        else [x]
                xs ->
                    if all isFuncCall xs
                        then [foldl1 (\a b -> Parser.FuncCall{funcName = "sequence", funcArgs = [a, b], funcPos = Parser.anyPosition}) xs]
                        else xs)
            grouped
    withShadowedVars shadowedVars $ do
        if length (filter isFuncCall exprs) == 1
            then concatMapM (`compileExpr` expectedType) exprs
            else concatMapM (`compileExpr` expectedType) nestedSequence
  where
    isFuncCall :: Parser.Expr -> Bool
    isFuncCall (Parser.FuncCall{funcName}) = funcName `notElem` internalFunctions
    isFuncCall _ = False
compileDoBlock _ expr _ = internalError $ "compileDoBlock: unexpected expr: " ++ show expr

compilePipeline :: CompileExpr -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
compilePipeline compileExpr (Parser.Pipeline{pipelineLhs = a, pipelineRhs = b}) expectedType =
    case b of
        Parser.Var{varName} -> compileExpr (Parser.FuncCall{funcName = varName, funcArgs = [a], funcPos = Parser.zeroPosition}) expectedType
        Parser.FuncCall{funcName = f, funcArgs} -> compileExpr (Parser.FuncCall{funcName = f, funcArgs = a : funcArgs, funcPos = Parser.zeroPosition}) expectedType
        Parser.Then{thenLhs = c, thenRhs = d} -> compileExpr (Parser.Then{thenLhs = Parser.Pipeline{pipelineLhs = a, pipelineRhs = c, pipelinePos = Parser.anyPosition}, thenRhs = d, thenPos = Parser.anyPosition}) expectedType
        _ -> internalError $ "compilePipeline: unexpected rhs: " ++ show b
compilePipeline _ expr _ = internalError $ "compilePipeline: unexpected expr: " ++ show expr

extractPatternVars :: Parser.Expr -> [String]
extractPatternVars (Parser.Var{varName}) = [varName]
extractPatternVars (Parser.ListPattern{listPatternExprs}) = concatMap extractPatternVars listPatternExprs
extractPatternVars _ = []

compilePatternMatch :: CompileExpr -> Parser.Expr -> String -> Parser.Type -> Compiler [Instruction]
compilePatternMatch compileExpr pat nextLabel expectedType = case pat of
    Parser.BoolLit{boolValue} -> do
        id' <- allocId
        let tmpName = "__pat_" ++ show id'
        return
            [ Dup
            , Push $ DBool boolValue
            , Eq
            , Jf nextLabel
            , Pop
            ]
    Parser.IntLit{intValue} -> do
        id' <- allocId
        return
            [ Dup
            , Push $ DInt $ fromIntegral intValue
            , Eq
            , Jf nextLabel
            , Pop
            ]
    Parser.StringLit{stringValue} -> do
        id' <- allocId
        return
            [ Dup
            , Push $ DString stringValue
            , Eq
            , Jf nextLabel
            , Pop
            ]
    Parser.Var{varName = "_"} ->
        return [Pop]
    Parser.Var{varName} -> do
        curCon <- gets currentContext
        modify (\s -> s{lets = Let{name = varName, vtype = Parser.Any, context = curCon} : lets s})
        return [LStore varName]
    Parser.ListPattern{listPatternExprs} -> do
        id' <- allocId
        let lenLabel = "__len_" ++ show id'
        lenCheck <- return
            [ Dup
            , Length
            , Push $ DInt $ fromIntegral $ length listPatternExprs
            , Eq
            , Jf nextLabel
            ]
        elemChecks <- mapM
            (\(i, elemPat) -> do
                elemInstrs <- compilePatternMatch compileExpr elemPat nextLabel expectedType
                return $ [Dup, Push $ DInt i, Index] ++ elemInstrs)
            (zip [0..] listPatternExprs)
        return $ lenCheck ++ concat elemChecks ++ [Pop]
    Parser.StructLit{structLitName, structLitFields} -> do
        nameCheck <- return
            [ Dup
            , Access "__name"
            , Push $ DString structLitName
            , Eq
            , Jf nextLabel
            ]
        fieldChecks <- mapM
            (\(fieldName, fieldPat) -> do
                fieldInstrs <- compilePatternMatch compileExpr fieldPat nextLabel expectedType
                return $ [Dup, Access fieldName] ++ fieldInstrs)
            structLitFields
        return $ nameCheck ++ concat fieldChecks ++ [Pop]
    _ -> do
        expr' <- compileExpr pat expectedType
        return $ [Dup] ++ expr' ++ [Eq, Jf nextLabel, Pop]

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = fmap concat (mapM f xs)

internalFunctions :: [String]
internalFunctions = ["unsafePrint", "unsafeGetLine", "unsafeGetChar", "unsafeRandom", "abs", "root", "sqrt"]
```

> **Note:** `compilePatternMatch` above is a reconstructed version from the exploration data. The actual implementation in `BytecodeCompiler.hs` (lines 747–853) may differ in details — read it carefully when moving the code and prefer copying verbatim over rewriting.

---

## Task 6: Create `Compiler.Modules`

**Files:**
- Create: `lib/Compiler/Modules.hs`

Move `compileImport` (including `mangleExprForImport` rewritten to use `traverseExprChildren`), `buildModuleMap`, `findSourceFile`, `preludeFile`, etc.

- [ ] **Step 1: Create `lib/Compiler/Modules.hs`**

The key cleanup here: replace the 26-case `mangleExprForImport` with `AST.traverseExprChildren`. The function only needs to actively transform a handful of node types (FuncDec, FuncDef, FuncCall name, StructLit name, Struct name, Trait name, Var name when in func list). All other nodes just recurse.

```haskell
module Compiler.Modules where

import AST (traverseExprChildren)
import AST qualified as Parser
import Compiler.State
import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Control.Monad.State (gets, modify)
import Data.Bifunctor (second)
import Data.Either (lefts, rights)
import Data.List (find, intercalate, isSuffixOf, nub, partition)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe, isJust, maybeToList)
import Data.Text qualified as T
import Parser (parseProgram)
import Parser qualified
import Paths_indigo qualified
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import System.FilePath
import VM (Instruction)

findSourceFile :: String -> [String] -> IO String
findSourceFile fileName paths = do
    dataFile <- Paths_indigo.getDataFileName fileName
    executablePathFile <- getExecutablePath >>= \x -> return $ takeDirectory x </> fileName
    let pathsToTry = map (</> fileName) (["/usr/local/lib/indigo/", "/usr/lib/indigo/"] ++ paths) ++ [executablePathFile, dataFile]
    firstThatExists <- firstM doesFileExist pathsToTry
    case firstThatExists of
        Just x -> return x
        Nothing -> error $ "Source file " ++ fileName ++ " not found. Tried:\n* " ++ intercalate "\n* " pathsToTry
  where
    firstM f [] = return Nothing
    firstM f (x:xs) = f x >>= \b -> if b then return (Just x) else firstM f xs

findSourceFile' :: String -> IO String
findSourceFile' fileName = findSourceFile fileName []

findFirstModuleFile :: [String] -> String -> IO (Maybe (String, String))
findFirstModuleFile [] _ = return Nothing
findFirstModuleFile (fileName : rest) sourcePath = do
    result <- try (findSourceFile fileName [sourcePath] >>= readFile) :: IO (Either SomeException String)
    case result of
        Right content -> return $ Just (fileName, content)
        Left _ -> findFirstModuleFile rest sourcePath

preludeFile :: IO String
preludeFile = findSourceFile' "std/prelude.in" >>= readFile

extractModuleName :: FilePath -> Parser.Program -> String
extractModuleName _ (Parser.Program _ (Just name)) = name
extractModuleName filePath _ =
    let base = takeBaseName filePath
        withoutExt = if ".in" `isSuffixOf` base then take (length base - 3) base else base
        dirPath = takeDirectory filePath
        pathParts = splitDirectories dirPath
        modulePath = filter (not . null) pathParts
     in if null modulePath then withoutExt else intercalate "." (modulePath ++ [withoutExt])

buildModuleMap :: [FilePath] -> IO (Either String (Map.Map String (Parser.Program, FilePath)))
buildModuleMap filePaths = do
    results <- mapM parseFile filePaths
    let errors = lefts results
    if not (null errors)
        then return $ Left (unlines errors)
        else do
            let programs = rights results
            let moduleNames = map (\(prog, path) -> extractModuleName path prog) programs
            let duplicates = findDuplicates moduleNames
            if not (null duplicates)
                then return $ Left $ "Duplicate module names: " ++ intercalate ", " duplicates
                else return $ Right $ Map.fromList $ zip moduleNames programs
  where
    parseFile path = do
        content <- readFile path
        case parseProgram (T.pack content) Parser.initCompilerFlags{Parser.needsMain = False} of
            Left err -> return $ Left $ "Parse error in " ++ path ++ ":\n" ++ show err
            Right prog -> return $ Right (prog, path)
    findDuplicates xs = [x | (x, count) <- map (\x -> (x, length (filter (== x) xs))) (nub xs), count > 1]

-- | Mangle expressions from an imported module by prefixing function and struct names.
-- Uses traverseExprChildren for structural recursion; only active cases need explicit handling.
mangleExprForImport :: Parser.Expr -> String -> [String] -> [String] -> Parser.Expr
mangleExprForImport expr prefix funcs structs = go expr
  where
    go :: Parser.Expr -> Parser.Expr
    -- Active transformations: these change names
    go (Parser.FuncDec{name, types, generics, funcDecPos}) =
        Parser.FuncDec{name = prefix ++ "." ++ name, types = map mangleType types, generics = generics, funcDecPos = funcDecPos}
    go (Parser.FuncDef{name, args, body, funcDefPos}) =
        Parser.FuncDef{name = prefix ++ "." ++ name, args = args, body = go body, funcDefPos = funcDefPos}
    go (Parser.Struct{name, fields, refinement, refinementSrc, is, isValueStruct, generics, structPos}) =
        Parser.Struct{name = prefix ++ "." ++ name, fields = map (second mangleType) fields, refinement = fmap go refinement, refinementSrc = refinementSrc, is = is, isValueStruct = isValueStruct, generics = generics, structPos = structPos}
    go (Parser.StructLit{structLitName, structLitFields, structLitTypeArgs, structLitPos}) =
        let mangledName = if structLitName `elem` structs then prefix ++ "." ++ structLitName else structLitName
         in Parser.StructLit{structLitName = mangledName, structLitFields = map (second go) structLitFields, structLitTypeArgs = structLitTypeArgs, structLitPos = structLitPos}
    go (Parser.Trait{name, methods, generics, requiredProperties, refinement, refinementSrc, traitPos}) =
        Parser.Trait{name = prefix ++ "." ++ name, methods = map go methods, generics = generics, requiredProperties = map (second mangleType) requiredProperties, refinement = fmap go refinement, refinementSrc = refinementSrc, traitPos = traitPos}
    go (Parser.Impl{trait, traitTypeArgs, for, methods, implPos}) =
        Parser.Impl{trait = trait, traitTypeArgs = traitTypeArgs, for = mangleType for, methods = map (mangleImplMethod . go) methods, implPos = implPos}
    go (Parser.FuncCall{funcName, funcArgs, funcPos}) =
        let mangledName = if funcName `elem` funcs then prefix ++ "." ++ funcName else funcName
         in Parser.FuncCall{funcName = mangledName, funcArgs = map go funcArgs, funcPos = funcPos}
    go (Parser.External{externalName, externalArgs, externalPos}) =
        Parser.External{externalName = prefix ++ "." ++ externalName, externalArgs = map go externalArgs, externalPos = externalPos}
    go (Parser.ParenApply{parenApplyExpr, parenApplyArgs, parenApplyPos}) =
        case parenApplyExpr of
            Parser.Var{varName} | varName `elem` funcs ->
                Parser.ParenApply{parenApplyExpr = Parser.Var{varName = prefix ++ "." ++ varName, varPos = Parser.anyPosition}, parenApplyArgs = map go parenApplyArgs, parenApplyPos = parenApplyPos}
            _ -> Parser.ParenApply{parenApplyExpr = go parenApplyExpr, parenApplyArgs = map go parenApplyArgs, parenApplyPos = parenApplyPos}
    -- Pass-through: use traverseExprChildren for structural recursion
    go e = runIdentity $ traverseExprChildren (return . go) e

    mangleType :: Parser.Type -> Parser.Type
    mangleType (Parser.StructT name typeArgs) =
        let mangledName = if name `elem` structs then prefix ++ "." ++ name else name
         in Parser.StructT mangledName (map mangleType typeArgs)
    mangleType (Parser.List t) = Parser.List (mangleType t)
    mangleType (Parser.Tuple ts) = Parser.Tuple (map mangleType ts)
    mangleType (Parser.Fn args ret) = Parser.Fn (map mangleType args) (mangleType ret)
    mangleType t = t

    mangleImplMethod :: Parser.Expr -> Parser.Expr
    mangleImplMethod (Parser.FuncDef{name, args, body, funcDefPos}) =
        Parser.FuncDef{name = name, args = args, body = go body, funcDefPos = funcDefPos}
    mangleImplMethod e = go e

runIdentity :: Identity a -> a
runIdentity (Identity a) = a

newtype Identity a = Identity {runIdentity' :: a}
instance Functor Identity where fmap f (Identity a) = Identity (f a)
instance Applicative Identity where pure = Identity; Identity f <*> Identity a = Identity (f a)

compileImport :: CompileExpr -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
compileImport compileExpr (Parser.Import{objects = o, from, as, qualified, importPos}) expectedType = do
    if o /= ["*"]
        then do
            cerror "Only * imports are supported right now" importPos
            return []
        else do
            modules' <- gets modules
            currentModule' <- gets currentModule
            if from `elem` maybeToList currentModule'
                then do
                    cerror ("Circular import detected: module '" ++ from ++ "' imports itself") importPos
                    return []
                else do
                    (importedProg, importedPath) <- case Map.lookup from modules' of
                        Just (prog, path) -> return (prog, path)
                        Nothing -> do
                            sourcePath' <- gets sourcePath
                            let srcDir = takeDirectory sourcePath'
                            let fileNamesToTry = [from ++ ".in", "std/" ++ from ++ ".in"]
                            fileResult <- liftIO $ findFirstModuleFile fileNamesToTry srcDir
                            case fileResult of
                                Nothing -> do
                                    cerror ("Module '" ++ from ++ "' not found. Tried: " ++ intercalate ", " fileNamesToTry) importPos
                                    return (Parser.Program [] Nothing, "")
                                Just (fileName, i) -> do
                                    case parseProgram (T.pack i) Parser.initCompilerFlags{Parser.needsMain = False} of
                                        Left err -> do
                                            cerror ("Parse error in imported module '" ++ from ++ "':\n" ++ show err) importPos
                                            return (Parser.Program [] Nothing, "")
                                        Right p -> return (p, fileName)
                    let (Parser.Program exprs _) = importedProg
                    let moduleNameToUse = fromMaybe from (Parser.moduleName importedProg)
                    let prefix =
                            if qualified || isJust as
                                then if qualified then moduleNameToUse else fromJust as
                                else ""
                    let definedFuncs = concatMap extractDefinedFuncNames exprs
                    let definedStructs = concatMap extractDefinedStructNames exprs
                    let exprsToCompile =
                            if null prefix
                                then exprs
                                else map (\e -> mangleExprForImport e prefix definedFuncs definedStructs) exprs
                    let (traitsAndStructsToCompile, otherExprsToCompile) = partition (\case Parser.Trait{} -> True; Parser.Struct{} -> True; _ -> False) exprsToCompile
                    previousSourceFile <- gets currentSourceFile
                    modify (\s -> s{currentSourceFile = importedPath})
                    _ <- concatMapM (`compileExpr` expectedType) traitsAndStructsToCompile
                    result <- concatMapM (`compileExpr` expectedType) otherExprsToCompile
                    modify (\s -> s{currentSourceFile = previousSourceFile})
                    return result
compileImport _ expr _ = internalError $ "compileImport: unexpected expr: " ++ show expr

extractDefinedFuncNames :: Parser.Expr -> [String]
extractDefinedFuncNames (Parser.FuncDec{name}) = [name]
extractDefinedFuncNames (Parser.FuncDef{name}) = [name]
extractDefinedFuncNames (Parser.Function{dec = Parser.FuncDec{name}}) = [name]
extractDefinedFuncNames (Parser.Let{letName}) = [letName]
extractDefinedFuncNames _ = []

extractDefinedStructNames :: Parser.Expr -> [String]
extractDefinedStructNames (Parser.Struct{name}) = [name]
extractDefinedStructNames _ = []

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = fmap concat (mapM f xs)
```

> **Important:** `traverseExprChildren` in `AST.hs` (line 76) has type `(Applicative f) => (Expr -> f Expr) -> Expr -> f Expr`. Using `Identity` as the functor gives us pure recursion. Verify this compiles correctly.

---

## Task 7: Create `Compiler.Literals`

**Files:**
- Create: `lib/Compiler/Literals.hs`

- [ ] **Step 1: Create `lib/Compiler/Literals.hs`**

```haskell
module Compiler.Literals where

import AST qualified as Parser
import Compiler.State
import VM

compileLiterals :: CompileExpr -> Parser.Expr -> Parser.Type -> Compiler (Maybe [Instruction])
compileLiterals compileExpr expr _ = case expr of
    Parser.IntLit{intValue = x} -> return $ Just [Push $ DInt $ fromIntegral x]
    Parser.StringLit{stringValue = x} -> return $ Just [Push $ DString x]
    Parser.FloatLit{floatValue} -> return $ Just [Push $ DFloat floatValue]
    Parser.DoubleLit{doubleValue} -> return $ Just [Push $ DDouble doubleValue]
    Parser.BoolLit{boolValue} -> return $ Just [Push $ DBool boolValue]
    Parser.CharLit{charValue = x} -> return $ Just [Push $ DChar x]
    Parser.UnaryMinus{unaryMinusExpr = Parser.FloatLit{floatValue = x}} -> return $ Just [Push $ DFloat (-x)]
    Parser.UnaryMinus{unaryMinusExpr = Parser.IntLit{intValue = x}} -> return $ Just [Push $ DInt $ -fromInteger x]
    _ -> return Nothing  -- not a literal, caller handles
```

---

## Task 8: Create `Compiler.Structs`

**Files:**
- Create: `lib/Compiler/Structs.hs`

Move `compileStruct`, `compileStructLit`, `compileStructAccess`, `compileCast`, `compileTupleLit`, `compileTupleAccess`, `validateTypeParameters`, `getStructFields`, `substituteGenerics`.

- [ ] **Step 1: Create `lib/Compiler/Structs.hs`**

Copy the relevant `compileExpr` clauses verbatim from `BytecodeCompiler.hs` (lines 1403–1563), wrapping each in a named function:

```haskell
module Compiler.Structs where

import AST (anyPosition, zeroPosition)
import AST qualified as Parser
import Compiler.Refinements (runRefinement, transformRefinementExpr)
import Compiler.State
import Compiler.Types (typeOf, typeToString, typeToData, structNameFromType)
import Control.Monad (forM_, unless, when)
import Control.Monad.State (gets, modify)
import Data.Bifunctor (second)
import Data.List (find, intercalate)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Foreign (nullPtr, ptrToWordPtr)
import VM

substituteGenerics :: Parser.Type -> [(String, Parser.Type)] -> Parser.Type
substituteGenerics (Parser.StructT name args) substitutions =
    case lookup name substitutions of
        Just t -> t
        Nothing -> Parser.StructT name (map (`substituteGenerics` substitutions) args)
substituteGenerics (Parser.List t) substitutions = Parser.List (substituteGenerics t substitutions)
substituteGenerics (Parser.Tuple ts) substitutions = Parser.Tuple (map (`substituteGenerics` substitutions) ts)
substituteGenerics (Parser.Fn args ret) substitutions = Parser.Fn (map (`substituteGenerics` substitutions) args) (substituteGenerics ret substitutions)
substituteGenerics t _ = t

validateTypeParameters :: [String] -> Parser.Type -> AST.Position -> String -> Compiler ()
-- (copy verbatim from BytecodeCompiler.hs — search for validateTypeParameters)

compileStruct :: CompileExpr -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
-- copy compileExpr clause for Parser.Struct verbatim (lines 1453-1477)

compileStructLit :: CompileExpr -> (Parser.Program -> Compiler (Either [Instruction] [CompilerError])) -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
-- copy compileExpr clause for Parser.StructLit verbatim (lines 1478-1556)
-- note: runRefinement needs compileProgram passed in to avoid circular dep

compileStructAccess :: CompileExpr -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
-- copy compileExpr clause for Parser.StructAccess verbatim (lines 1557-1563)

compileCast :: CompileExpr -> (Parser.Program -> Compiler (Either [Instruction] [CompilerError])) -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
-- copy compileExpr clause for Parser.Cast verbatim (lines 1403-1452)
```

> **Copy verbatim** — do not rewrite logic. The function signatures above show how to thread `compileProgram` for refinement checking.

---

## Task 9: Create `Compiler.Traits` and `Compiler.Functions`

**Files:**
- Create: `lib/Compiler/Traits.hs`
- Create: `lib/Compiler/Functions.hs`

These are the two largest submodules. Copy verbatim from `BytecodeCompiler.hs`.

- [ ] **Step 1: Create `lib/Compiler/Traits.hs`**

Move `compileTrait` (lines 1699–1703), `compileImpl` (lines 1704–1786), `createVirtualFunctions` (lines 1988–2023).

```haskell
module Compiler.Traits where

import AST (anyPosition)
import AST qualified as Parser
import Compiler.State
import Compiler.Structs (substituteGenerics)
import Compiler.Types (typeToString, structNameFromType)
-- ... remaining imports

compileTrait :: CompileExpr -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
-- copy verbatim

compileImpl :: CompileExpr -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
-- copy verbatim

createVirtualFunctions :: Compiler ()
-- copy verbatim
```

- [ ] **Step 2: Create `lib/Compiler/Functions.hs`**

This is the most complex submodule. Move:
- `compileFuncDec` (lines 1183–1188)
- `compileFuncDef` (lines 1189–1261) including `compileParameter`
- `compileFunction` (lines 1307–1319)
- `compileFuncCall` (lines 935–1182) — the main FuncCall handler
- `compileLambda` (lines 1790–1801)
- `compileVar` (lines 1276–1286)
- `compileLet` (lines 1287–1306)
- `compileParenApply` (lines 1267–1275)
- `compileExternal` (lines 1810–1814)
- `compileBinOp` (unified from `compileBinOp`, `compileBinOpSeq`, `doBinOp`)
- `unsafeBuiltins` map
- `findFunction`, `findAnyFunction`, `typesMatch`, `typesMatchExactly`
- `internalFunctions`
- `getBinOpPosition`, `extractPosition`
- `unmangleFunctionName`

```haskell
module Compiler.Functions where

import AST qualified as Parser
import Compiler.State
import Compiler.Types
-- ... imports

-- | Lookup table for unsafe builtin functions that map directly to VM instructions.
unsafeBuiltins :: Map.Map String ([Parser.Expr] -> CompileExpr -> Parser.Type -> Compiler [Instruction])
unsafeBuiltins = Map.fromList
    [ ("unsafeAdd",    binOp Add)
    , ("unsafeSub",    binOp Sub)
    , ("unsafeMul",    binOp Mul)
    , ("unsafeDiv",    binOp Div)
    , ("unsafeMod",    binOp Mod)
    , ("unsafePow",    binOp Pow)
    , ("unsafeGt",     binOp Gt)
    , ("unsafeLt",     binOp Lt)
    , ("unsafeGe",     \[x,y] ce et -> ce x et >>= \x' -> ce y et >>= \y' -> return (x' ++ y' ++ [Lt, Not]))
    , ("unsafeLe",     \[x,y] ce et -> ce x et >>= \x' -> ce y et >>= \y' -> return (x' ++ y' ++ [Gt, Not]))
    , ("unsafeEq",     binOp Eq)
    , ("unsafeNeq",    \[x,y] ce et -> ce x et >>= \x' -> ce y et >>= \y' -> return (x' ++ y' ++ [Eq, Not]))
    , ("unsafeAnd",    binOp And)
    , ("unsafeOr",     binOp Or)
    , ("unsafePrint",  \[x] ce et -> ce x et >>= \x' -> return (x' ++ [Builtin Print]))
    , ("unsafeGetLine",  \[] _ _ -> return [Builtin GetLine])
    , ("unsafeGetChar",  \[] _ _ -> return [Builtin GetChar])
    , ("unsafeRandom",   \[] _ _ -> return [Builtin Random])
    , ("unsafeListAdd",  \[y,x] ce et -> ce x et >>= \x' -> ce y et >>= \y' -> return (x' ++ y' ++ [ListAdd 2]))
    , ("unsafeListIndex",\[x,y] ce et -> ce x et >>= \x' -> ce y et >>= \y' -> return (x' ++ y' ++ [Index]))
    , ("unsafeListLength",\[x] ce et -> ce x et >>= \x' -> return (x' ++ [Length]))
    , ("unsafeListSlice",\[x,y,z] ce et -> ce x et >>= \x' -> ce y et >>= \y' -> ce z et >>= \z' -> return (x' ++ y' ++ z' ++ [Slice]))
    , ("unsafeStructAccess", binOp AAccess)
    , ("unsafeKeys",   \[x] ce et -> ce x et >>= \x' -> return (x' ++ [Keys]))
    , ("unsafeUpdate", \[x,y,z] ce et -> ce x et >>= \x' -> ce y et >>= \y' -> ce z et >>= \z' -> return (x' ++ y' ++ z' ++ [Update]))
    , ("unsafeExit",   \[x] ce et -> ce x et >>= \x' -> return (x' ++ [Exit]))
    , ("abs",          \[x] ce et -> ce x et >>= \x' -> return (x' ++ [Abs]))
    ]
  where
    binOp :: Instruction -> [Parser.Expr] -> CompileExpr -> Parser.Type -> Compiler [Instruction]
    binOp instr [x, y] compileExpr expectedType =
        compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [instr])
    binOp _ _ _ _ = internalError "binOp: wrong number of arguments"
```

> **Note on `compileBinOp` unification:** The current code has `compileBinOp` (calls `doBinOp`), `compileBinOpSeq` (inline variant for `>=`/`<=`), and `doBinOp` (core logic). Unify by keeping the existing `compileBinOp` and `doBinOp` signatures but inlining `compileBinOpSeq` into `doBinOp` as an optional `[Instruction]` argument (or a separate internal helper). The important constraint: the generated instructions must be identical to the current output. Verify with tests before and after.

---

## Task 10: Update `indigo.cabal`

**Files:**
- Modify: `indigo.cabal`

- [ ] **Step 1: Add new modules to `other-modules` in the `library` stanza**

In `indigo.cabal`, find the `library` stanza and change:

```cabal
    other-modules:      Paths_indigo
```

to:

```cabal
    other-modules:
        Paths_indigo
        Compiler.State
        Compiler.Types
        Compiler.Literals
        Compiler.Functions
        Compiler.Structs
        Compiler.Traits
        Compiler.ControlFlow
        Compiler.Lists
        Compiler.Modules
        Compiler.Refinements
```

- [ ] **Step 2: Add `Data.List.NonEmpty` if used for contextStack (only if NonEmpty is adopted)**

Check if `base` already provides `Data.List.NonEmpty` — it does since GHC 7.10, so no new dependency needed.

---

## Task 11: Rewrite `BytecodeCompiler.hs` as Thin Orchestrator

**Files:**
- Modify: `lib/BytecodeCompiler.hs`

This is the integration step. Replace the 2,096-line file with a ~150-line orchestrator.

- [ ] **Step 1: Rewrite `lib/BytecodeCompiler.hs`**

```haskell
{-# LANGUAGE CPP #-}

module BytecodeCompiler (
    module Compiler.State,
    module Compiler.Types,
    compileExpr,
    compileProgram,
    compileProgramBare,
    compileDry,
    compileFail,
    renderCompilerErrors,
    buildModuleMap,
    locateLabel,
    preludeExpr,
) where

import AST qualified as Parser
import Compiler.ControlFlow
import Compiler.Functions
import Compiler.Lists
import Compiler.Literals
import Compiler.Modules
import Compiler.Refinements
import Compiler.State
import Compiler.Structs
import Compiler.Traits
import Compiler.Types
import Control.Monad.State (StateT, evalStateT, gets, modify)
import Data.List (find)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import ErrorRenderer (SourceError (..), parseErrorBundleToSourceErrors, renderErrors)
import Parser (CompilerFlags (CompilerFlags), parseProgram)
import Parser qualified
import VM

compileExpr :: Parser.Expr -> Parser.Type -> Compiler [Instruction]
compileExpr expr expectedType = case expr of
    -- Literals
    Parser.IntLit{} -> compileLiterals compileExpr expr expectedType >>= \case Just r -> return r; Nothing -> internalError "compileLiterals missed IntLit"
    Parser.StringLit{} -> compileLiterals compileExpr expr expectedType >>= \case Just r -> return r; Nothing -> internalError "compileLiterals missed StringLit"
    Parser.FloatLit{} -> compileLiterals compileExpr expr expectedType >>= \case Just r -> return r; Nothing -> internalError "compileLiterals missed FloatLit"
    Parser.DoubleLit{} -> compileLiterals compileExpr expr expectedType >>= \case Just r -> return r; Nothing -> internalError "compileLiterals missed DoubleLit"
    Parser.BoolLit{} -> compileLiterals compileExpr expr expectedType >>= \case Just r -> return r; Nothing -> internalError "compileLiterals missed BoolLit"
    Parser.CharLit{} -> compileLiterals compileExpr expr expectedType >>= \case Just r -> return r; Nothing -> internalError "compileLiterals missed CharLit"
    Parser.UnaryMinus{unaryMinusExpr = Parser.FloatLit{}} -> compileLiterals compileExpr expr expectedType >>= \case Just r -> return r; Nothing -> internalError "compileLiterals missed UnaryMinus Float"
    Parser.UnaryMinus{unaryMinusExpr = Parser.IntLit{}} -> compileLiterals compileExpr expr expectedType >>= \case Just r -> return r; Nothing -> internalError "compileLiterals missed UnaryMinus Int"
    -- Arithmetic (binary ops)
    Parser.Add{addLhs = x, addRhs = y} -> compileBinOp compileExpr "+" x y expectedType Add
    Parser.Sub{subLhs = x, subRhs = y} -> compileBinOp compileExpr "-" x y expectedType Sub
    Parser.Mul{mulLhs = x, mulRhs = y} -> compileBinOp compileExpr "*" x y expectedType Mul
    Parser.Div{divLhs = x, divRhs = y} -> compileBinOp compileExpr "/" x y expectedType Div
    Parser.Modulo{moduloLhs = x, moduloRhs = y} -> compileBinOp compileExpr "%" x y expectedType Mod
    Parser.Power{powerBase = x, powerExponent = y} -> compileBinOp compileExpr "^" x y expectedType Pow
    Parser.Gt{gtLhs = x, gtRhs = y} -> compileBinOp compileExpr ">" x y expectedType Gt
    Parser.Lt{ltLhs = x, ltRhs = y} -> compileBinOp compileExpr "<" x y expectedType Lt
    Parser.Ge{geLhs = x, geRhs = y} -> compileBinOpSeq compileExpr ">=" x y expectedType [Lt, Not]
    Parser.Le{leLhs = x, leRhs = y} -> compileBinOpSeq compileExpr "<=" x y expectedType [Gt, Not]
    Parser.Eq{eqLhs = x, eqRhs = y} -> compileBinOp compileExpr "==" x y expectedType Eq
    Parser.Neq{neqLhs = x, neqRhs = y} -> compileBinOp compileExpr "!=" x y expectedType Neq
    Parser.And{andLhs = x, andRhs = y} -> compileBinOp compileExpr "&&" x y expectedType And
    Parser.Or{orLhs = x, orRhs = y} -> compileBinOp compileExpr "||" x y expectedType Or
    Parser.Not{notExpr = x} -> compileExpr x expectedType >>= \x' -> return (x' ++ [Not])
    Parser.UnaryMinus{unaryMinusExpr = x} -> compileExpr x expectedType >>= \x' -> return (x' ++ [Push $ DInt (-1), Mul])
    -- Control flow
    Parser.If{} -> compileIf compileExpr expr expectedType
    Parser.When{} -> compileWhen compileExpr expr expectedType
    Parser.DoBlock{} -> compileDoBlock compileExpr expr expectedType
    Parser.Pipeline{} -> compilePipeline compileExpr expr expectedType
    Parser.Then{thenLhs = a, thenRhs = b} -> do
        a' <- compileExpr a expectedType
        b' <- compileExpr b expectedType
        return $ a' ++ b'
    Parser.Flexible{flexibleExpr = a} -> compileExpr a expectedType >>= \a' -> return $ Meta "flex" : a'
    Parser.StrictEval{strictEvalExpr = e} -> compileExpr e expectedType
    -- Lists
    Parser.ListLit{} -> compileLists compileExpr expr expectedType
    Parser.TupleLit{} -> compileLists compileExpr expr expectedType
    Parser.TupleAccess{} -> compileLists compileExpr expr expectedType
    Parser.ListConcat{} -> compileLists compileExpr expr expectedType
    Parser.ListAdd{} -> compileLists compileExpr expr expectedType
    -- Functions
    Parser.FuncCall{} -> compileFuncCall compileExpr expr expectedType
    Parser.FuncDec{} -> compileFuncDec compileExpr expr expectedType
    Parser.FuncDef{} -> compileFuncDef compileExpr expr expectedType
    Parser.Function{} -> compileFunction compileExpr expr expectedType
    Parser.Lambda{} -> compileLambda compileExpr expr expectedType
    Parser.Var{} -> compileVar compileExpr expr expectedType
    Parser.Let{} -> compileLet compileExpr expr expectedType
    Parser.ParenApply{} -> compileParenApply compileExpr expr expectedType
    Parser.Placeholder _ -> return []
    -- Structs
    Parser.Struct{} -> compileStruct compileExpr expr expectedType
    Parser.StructLit{} -> compileStructLit compileExpr compileProgram expr expectedType
    Parser.StructAccess{} -> compileStructAccess compileExpr expr expectedType
    Parser.Cast{} -> compileCast compileExpr compileProgram expr expectedType
    -- Traits
    Parser.Trait{} -> compileTrait compileExpr expr expectedType
    Parser.Impl{} -> compileImpl compileExpr expr expectedType
    -- Modules
    Parser.Import{} -> compileImport compileExpr expr expectedType
    -- External FFI
    Parser.External{externalArgs = []} -> return []
    Parser.External{} -> compileExternal compileExpr expr expectedType
    -- Catch-all
    x -> internalError $ show x ++ " is not implemented in compileExpr"

preludeExpr :: IO [Parser.Expr]
preludeExpr = do
    i <- preludeFile
    case parseProgram (T.pack i) CompilerFlags{verboseMode = False, needsMain = False} of
        Left err -> error $ "Parse error:\n" ++ renderErrors (parseErrorBundleToSourceErrors err (T.pack i)) i
        Right prog@(Parser.Program progExpr _) -> do
            _ <-
                compileDry prog "<prelude>" >>= \case
                    Right err -> compileFail "<prelude>" err (Map.singleton "<prelude>" i) >> error ""
                    Left p' -> return p'
            return $ progExpr ++ [Parser.FuncDef "__sep" [] (Parser.Placeholder Parser.anyPosition) Parser.anyPosition]

compileProgram :: Parser.Program -> Compiler (Either [Instruction] [CompilerError])
compileProgram (Parser.Program expr _) = do
    prelude <- liftIO preludeExpr
    prelude' <- concatMapM (`compileExpr` Parser.Any) prelude
    freePart <- concatMapM (`compileExpr` Parser.Any) expr
    createVirtualFunctions
    funcs <- gets functions
    let functions' = concatMap function (reverse funcs)
    errors' <- gets errors
    let hasTopLevelLets = any (\case Parser.Let{} -> True; _ -> False) expr
    if null errors'
        then return $ Left $ prelude' ++ freePart ++ ([Jmp "main" | hasTopLevelLets]) ++ functions' ++ [Push $ DInt 0, Exit]
        else return $ Right errors'

compileProgramBare :: Parser.Program -> Compiler (Either [Instruction] [CompilerError])
compileProgramBare (Parser.Program expr _) = do
    freePart <- concatMapM (`compileExpr` Parser.Any) expr
    functions' <- gets functions >>= \x -> return $ concatMap function (reverse x)
    errors' <- gets errors
    if null errors'
        then return $ Left $ functions' ++ freePart ++ [Push $ DInt 0, Exit]
        else return $ Right errors'

compileDry :: Parser.Program -> String -> IO (Either [Instruction] [CompilerError])
compileDry prog sourcePath' =
    evalStateT (compileProgramBare prog) (initCompilerStateWithFile prog sourcePath')

locateLabel :: [Instruction] -> String -> Int
locateLabel program label =
    case elemIndex (Label label) program of
        Just x -> x
        Nothing -> internalError $ "Label " ++ label ++ " not found"

groupErrorsByFile :: [CompilerError] -> Map.Map String [CompilerError]
groupErrorsByFile errors = Map.fromListWith (++) $ map (\e@(CompilerError _ _ file) -> (file, [e])) errors

compilerErrorToSourceError :: CompilerError -> SourceError
compilerErrorToSourceError (CompilerError msg pos _) = SourceError{errorMessage = msg, errorPosition = pos}

renderCompilerErrors :: [CompilerError] -> Map.Map String String -> String
renderCompilerErrors errors fileContents =
    let errorsByFile = groupErrorsByFile errors
        hasMultipleFiles = Map.size errorsByFile > 1
     in unlines $
            Map.elems $
                Map.mapWithKey
                    (\filePath fileErrors ->
                        let content = Map.findWithDefault "" filePath fileContents
                            sourceErrors = map compilerErrorToSourceError fileErrors
                         in if null content
                                then (if hasMultipleFiles then filePath ++ ":\n" else "") ++ unlines (map (\(CompilerError msg _ _) -> msg) fileErrors)
                                else (if hasMultipleFiles then filePath ++ ":\n" else "") ++ renderErrors sourceErrors content)
                    errorsByFile

compileFail :: String -> [CompilerError] -> Map.Map String String -> IO ()
compileFail fileName errors fileContents = do
    allFileContents <- readMissingFileContents errors fileContents
    let rendered = renderCompilerErrors errors allFileContents
    let hasMultipleFiles = Map.size (groupErrorsByFile errors) > 1
    let output = if hasMultipleFiles then rendered else "\x1b[1m\x1b[36m" ++ fileName ++ "\x1b[0m \n" ++ rendered
    putStrLn output
  where
    readMissingFileContents errs existing = do
        let errorFiles = nub $ map (\(CompilerError _ _ file) -> file) errs
        let missingFiles = filter (\file -> not (Map.member file existing) && file /= "" && file /= "no file") errorFiles
        additionalContents <-
            Map.fromList
                <$> mapM
                    (\file -> do
                        content <- try (readFile file) :: IO (Either SomeException String)
                        case content of
                            Right c -> return (file, c)
                            Left _ -> return (file, ""))
                    missingFiles
        return $ Map.union existing additionalContents

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = fmap concat (mapM f xs)
```

- [ ] **Step 2: Build to check it compiles**

```bash
cabal build indigo 2>&1 | head -60
```

Expected: No errors. If there are, fix import issues before proceeding.

---

## Task 12: Fix All Compilation Errors

**Files:**
- Modify: whichever submodule files have errors

After the initial build attempt, there will likely be missing imports, name mismatches, or pattern match issues. Work through them systematically:

- [ ] **Step 1: Run the build and collect all errors**

```bash
cabal build indigo 2>&1
```

- [ ] **Step 2: Fix errors one module at a time, starting from `Compiler.State` outward**

Common issues to expect:
- Missing `import Control.Monad.State (liftIO)` in modules that do IO
- `GHC.Generics (Generic)` needed for `Function` if `deriving Generic` is kept
- `Data.List.elemIndex` needed in `BytecodeCompiler.hs` for `locateLabel`
- `concatMapM` defined in multiple modules — pick one canonical location (e.g., `Compiler.State`) and import it everywhere else
- `implsFor` used in `Compiler.Types` but defined in `Compiler.State` — ensure the import is present
- `internalFunctions` referenced in `Compiler.ControlFlow` and `Compiler.Functions` — keep it in `Compiler.State` or `Compiler.Functions` and import where needed

- [ ] **Step 3: Re-run build after each fix batch until clean**

```bash
cabal build indigo 2>&1 | head -40
```

---

## Task 13: Run Tests and Verify Correctness

**Files:** None — verification only.

- [ ] **Step 1: Run the full test suite**

```bash
cabal test indigo-test 2>&1
```

Expected: All tests pass. If any fail, they indicate a behavioral regression in the refactor — not a compilation issue. Investigate by comparing the failing case's generated bytecode against the original.

- [ ] **Step 2: Run the end-to-end smoke test**

```bash
cabal run indigo -- examples/fib.in 2>&1
```

Expected: Same output as before the refactor.

- [ ] **Step 3: Run the LSP tests**

```bash
cabal test indigo-lsp-test 2>&1
```

Expected: All pass (LSP imports from `BytecodeCompiler` which re-exports everything).

- [ ] **Step 4: Check formatting**

```bash
fourmolu --mode check . 2>&1
```

If there are formatting issues:

```bash
fourmolu --mode inplace .
```

- [ ] **Step 5: Run hlint**

```bash
hlint lib/ app/ 2>&1
```

Fix any issues flagged. Common: unused imports, redundant lambdas.

- [ ] **Step 6: Build with -Werror**

```bash
cabal build --ghc-options=-Werror 2>&1
```

Expected: Clean build with no warnings.

- [ ] **Step 7: Commit**

```bash
git add lib/Compiler/ lib/BytecodeCompiler.hs indigo.cabal
git commit -m "Refactor BytecodeCompiler into focused submodules

Split 2096-line BytecodeCompiler.hs into 10 submodules under lib/Compiler/:
State, Types, Literals, Functions, Structs, Traits, ControlFlow, Lists,
Modules, Refinements. BytecodeCompiler.hs becomes a thin orchestrator.

Cleanups bundled:
- unsafe builtins replaced with dispatch Map
- shadow/restore logic extracted to withShadowedVars
- mangleExprForImport simplified via traverseExprChildren
- internalError helper distinguishes compiler bugs from user errors
- currentContext/contextPath unified into contextStack

Public API unchanged; all callers compile without modification."
```

---

## Self-Review

**Spec coverage check:**

| Spec requirement | Covered by |
|-----------------|-----------|
| Split by expression type into 10 submodules | Tasks 1–9 |
| `compileExpr` as dispatch table in orchestrator | Task 11 |
| `CompileExpr` callback to avoid circular imports | Tasks 1, 4–9 |
| Public API unchanged | Task 11 (re-exports) |
| Cabal `other-modules` updated | Task 10 |
| Unsafe builtins → Map | Task 9 (`Compiler.Functions`) |
| `withShadowedVars` extracted | Task 5 (`Compiler.ControlFlow`) |
| Binary op helpers unified | Task 9 (`Compiler.Functions`) |
| `mangleExprForImport` → `traverseExprChildren` | Task 6 (`Compiler.Modules`) |
| `contextStack` replaces `currentContext`+`contextPath` | Task 1 (`Compiler.State`) |
| `internalError` helper | Task 1 (`Compiler.State`) |
| Verification: build, test, format, lint | Task 13 |

**Placeholder scan:** Tasks 8 and 9 say "copy verbatim" for the larger handlers — this is intentional direction, not a placeholder. The exact code is in `BytecodeCompiler.hs` at the line numbers cited; copying it is the correct action.

**Type consistency:** `CompileExpr` defined in `Compiler.State`, used uniformly across all submodule signatures. `Compiler a` alias used throughout. `internalError` defined in `Compiler.State`.

**Scope check:** This is one coherent refactoring. No decomposition needed.
