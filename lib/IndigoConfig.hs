module IndigoConfig where

import Data.Aeson (FromJSON (..), Value (..), (.:?))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist)
import System.FilePath

data IndigoConfig = IndigoConfig
    { mainFile :: Maybe FilePath
    , sourceDirs :: [FilePath]
    }
    deriving (Show, Eq)

instance FromJSON IndigoConfig where
    parseJSON (Object v) =
        IndigoConfig
            <$> v .:? "main"
            <*> (fromMaybe [] <$> v .:? "sourceDirs")
    parseJSON _ = fail "Expected object for IndigoConfig"

defaultConfig :: IndigoConfig
defaultConfig = IndigoConfig{mainFile = Nothing, sourceDirs = []}

loadConfig :: FilePath -> IO (Maybe IndigoConfig)
loadConfig configPath = do
    exists <- doesFileExist configPath
    if not exists
        then return Nothing
        else do
            content <- BS.readFile configPath
            case Aeson.eitherDecodeStrict content of
                Left _ -> return Nothing
                Right config -> return $ Just config

findConfigFile :: FilePath -> IO (Maybe FilePath)
findConfigFile startDir = do
    let configPath = startDir </> "indigo.json"
    exists <- doesFileExist configPath
    if exists
        then return $ Just configPath
        else do
            let parent = takeDirectory startDir
            if parent == startDir
                then return Nothing
                else findConfigFile parent
