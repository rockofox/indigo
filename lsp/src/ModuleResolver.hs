module ModuleResolver where

import AST (Expr (..), Program (..))
import BytecodeCompiler (extractModuleName)
import Control.Monad (filterM)
import Data.Either (rights)
import Data.List (nub)
import Data.Map qualified as Map
import Data.Text qualified as T
import IndigoConfig (IndigoConfig (..), findConfigFile, loadConfig, sourceDirs)
import Parser (initCompilerFlags, needsMain, parseProgram)
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , listDirectory
    )
import System.FilePath

type ModuleCache = Map.Map String (Program, FilePath)

data ModuleResolver = ModuleResolver
    { cache :: ModuleCache
    , configDir :: FilePath
    , config :: Maybe IndigoConfig
    }

initModuleResolver :: FilePath -> IO ModuleResolver
initModuleResolver workspaceRoot = do
    configFile <- findConfigFile workspaceRoot
    config <- case configFile of
        Just path -> loadConfig path
        Nothing -> return Nothing
    cache <- buildModuleCache workspaceRoot config
    return $ ModuleResolver{cache, configDir = workspaceRoot, config}

buildModuleCache :: FilePath -> Maybe IndigoConfig -> IO ModuleCache
buildModuleCache workspaceRoot maybeConfig = do
    let searchDirs = case maybeConfig of
            Just cfg -> if null (sourceDirs cfg) then [workspaceRoot] else map (workspaceRoot </>) (sourceDirs cfg)
            Nothing -> [workspaceRoot]
    allFiles <- concatMapM findIndigoFiles searchDirs
    results <- mapM parseModuleFile allFiles
    let programs = rights results
    let moduleNames = map (\(prog, path) -> extractModuleName path prog) programs
    let duplicates = findDuplicates moduleNames
    if not (null duplicates)
        then return Map.empty
        else return $ Map.fromList $ zip moduleNames programs
  where
    findDuplicates :: [String] -> [String]
    findDuplicates xs = [x | (x, count) <- map (\x -> (x, length (filter (== x) xs))) (nub xs), count > 1]

findIndigoFiles :: FilePath -> IO [FilePath]
findIndigoFiles dir = do
    exists <- doesDirectoryExist dir
    if not exists
        then return []
        else do
            entries <- listDirectory dir
            let paths = map (dir </>) entries
            files <- filterM doesFileExist $ filter ((== ".in") . takeExtension) paths
            dirs <- filterM doesDirectoryExist paths
            subFiles <- concatMapM findIndigoFiles dirs
            return $ files ++ subFiles

parseModuleFile :: FilePath -> IO (Either String (Program, FilePath))
parseModuleFile path = do
    content <- readFile path
    case parseProgram (T.pack content) initCompilerFlags{needsMain = False} of
        Left err -> return $ Left $ "Parse error in " ++ path ++ ":\n" ++ show err
        Right prog -> return $ Right (prog, path)

resolveModule :: ModuleResolver -> String -> Maybe (Program, FilePath)
resolveModule resolver moduleName = Map.lookup moduleName (cache resolver)

resolveModuleFromPath :: ModuleResolver -> String -> FilePath -> IO (Maybe (Program, FilePath))
resolveModuleFromPath resolver moduleName currentFile = do
    case resolveModule resolver moduleName of
        Just result -> return $ Just result
        Nothing -> do
            let currentDir = takeDirectory currentFile
            let fileNamesToTry = [moduleName ++ ".in", "std/" ++ moduleName ++ ".in"]
            result <- findFirstModuleFile fileNamesToTry currentDir
            case result of
                Just (fileName, content) -> do
                    case parseProgram (T.pack content) initCompilerFlags{needsMain = False} of
                        Left _ -> return Nothing
                        Right prog -> return $ Just (prog, fileName)
                Nothing -> return Nothing

findFirstModuleFile :: [String] -> FilePath -> IO (Maybe (String, String))
findFirstModuleFile [] _ = return Nothing
findFirstModuleFile (fileName : rest) sourcePath = do
    let fullPath = sourcePath </> fileName
    exists <- doesFileExist fullPath
    if exists
        then do
            content <- readFile fullPath
            return $ Just (fileName, content)
        else findFirstModuleFile rest sourcePath

extractImports :: [Expr] -> [Expr]
extractImports = filter isImport
  where
    isImport Import{} = True
    isImport _ = False

getImportedSymbols :: Program -> [String]
getImportedSymbols (Program exprs _) = concatMap extractSymbols exprs
  where
    extractSymbols expr = case expr of
        FuncDef{name} -> [name]
        FuncDec{name} -> [name]
        Function{dec = FuncDec{name}} -> [name]
        Struct{name} -> [name]
        Let{letName} -> [letName]
        _ -> []

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs




