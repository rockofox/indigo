module Compiler.State where

import AST (Position (..), zeroPosition)
import AST qualified as Parser
import Control.Monad (unless)
import Control.Monad.State (StateT, gets, modify)
import Data.Map qualified as Map
import GHC.Generics (Generic)
import VM (Instruction)

type Compiler a = StateT (CompilerState ()) IO a

type CompileExpr = Parser.Expr -> Parser.Type -> Compiler [Instruction]

data Function = Function
    { baseName :: String
    , funame :: String
    , function :: [Instruction]
    , types :: [Parser.Type]
    , context :: String
    }
    deriving (Show, Generic)

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

{- | Main compiler state.

Note: 'currentContext' and 'contextPath' are smart accessor functions,
not record fields. The single source of truth is 'contextStack'.
-}
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
    , contextStack :: [String]
    , externals :: [External]
    , functionsByTrait :: [(String, String, String, Parser.Expr)]
    , errors :: [CompilerError]
    , sourcePath :: String
    , skipRefinementCheck :: Bool
    , modules :: Map.Map String (Parser.Program, FilePath)
    , currentModule :: Maybe String
    , currentSourceFile :: String
    , nextFunctionLabels :: Map.Map String String
    }

currentContext :: CompilerState a -> String
currentContext s = case contextStack s of
    (c : _) -> c
    [] -> "__outside"

contextPath :: CompilerState a -> [String]
contextPath = contextStack

pushContext :: String -> CompilerState a -> CompilerState a
pushContext ctx s = s{contextStack = ctx : contextStack s}

popContext :: CompilerState a -> CompilerState a
popContext s = case contextStack s of
    [] -> s
    [_] -> s
    (_ : rest) -> s{contextStack = rest}

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
        , nextFunctionLabels = Map.empty
        }

initCompilerStateWithFile :: Parser.Program -> String -> CompilerState a
initCompilerStateWithFile prog sourcePath' =
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
        , sourcePath = sourcePath'
        , skipRefinementCheck = False
        , modules = Map.empty
        , currentModule = Nothing
        , currentSourceFile = sourcePath'
        , nextFunctionLabels = Map.empty
        }

initCompilerStateWithModules :: Map.Map String (Parser.Program, FilePath) -> Parser.Program -> String -> CompilerState a
initCompilerStateWithModules modules' prog sourcePath' =
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
        , sourcePath = sourcePath'
        , skipRefinementCheck = False
        , modules = modules'
        , currentModule = Parser.moduleName prog
        , currentSourceFile = sourcePath'
        , nextFunctionLabels = Map.empty
        }

cerror :: String -> AST.Position -> Compiler ()
cerror msg pos = do
    currentFile <- gets currentSourceFile
    unless (pos == AST.zeroPosition) $
        modify (\s -> s{errors = errors s ++ [CompilerError msg pos currentFile]})

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
    let matching = filter (\x -> structNameFromType (Parser.for x) == structName) impls'
    return $ map Parser.trait matching

structNameFromType :: Parser.Type -> String
structNameFromType (Parser.StructT n _) = n
structNameFromType Parser.None = "None"
structNameFromType (Parser.List _) = "List"
structNameFromType (Parser.Tuple _) = "Tuple"
structNameFromType Parser.Any = "Any"
structNameFromType t = internalError $ "structNameFromType: expected StructT, got " ++ show t

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

internalFunctions :: [String]
internalFunctions =
    [ "unsafePrint"
    , "unsafeGetLine"
    , "unsafeGetChar"
    , "unsafeRandom"
    , "unsafeOpenFile"
    , "unsafeReadFile"
    , "unsafeWriteFile"
    , "unsafeCloseFile"
    , "unsafeSocket"
    , "unsafeBind"
    , "unsafeListen"
    , "unsafeAccept"
    , "unsafeConnect"
    , "unsafeSend"
    , "unsafeRecv"
    , "unsafeCloseSocket"
    , "unsafeListAdd"
    , "unsafeListLength"
    , "unsafeListSlice"
    , "unsafeKeys"
    , "unsafeUpdate"
    , "unsafeExit"
    , "abs"
    , "root"
    , "sqrt"
    ]
