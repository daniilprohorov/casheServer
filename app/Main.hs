{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Client
import           Data.Aeson
import           Data.Text               (Text)
import qualified Data.Yaml               as Y
import           GHC.Generics
import           Server
import           System.Cron
import           System.Console.CmdArgs

data Config = Config
    { portServer :: Int
    , apiKey     :: Text
    , apiRoot    :: Text
    , apiPath    :: Text
    , apiPort    :: Int
    , locations  :: [Text]
    , period     :: Text
    }
    deriving (Show, Generic)

data CasheServer = CasheServer
    { filepath :: FilePath
    }
    deriving (Show, Data, Typeable)

argsDescribe =
    CasheServer
        { filepath = "config.yaml" &= name "f" &= help "Config path (default: 'config.yaml')"
        }

instance FromJSON Config

main :: IO ()
main = do
    CasheServer{filepath} <- cmdArgs argsDescribe
    Config{portServer, apiKey, apiRoot, apiPath, apiPort, locations, period} <- Y.decodeFileThrow filepath
    let clientApiInfo = ClientApiInfo apiKey apiRoot apiPath apiPort
    let apiFunc = weatherRequest clientApiInfo
    storage <- initStorage locations
    let serverData = ServerData portServer apiFunc storage
    _ <- execSchedule $ do
        addJob (updateStorage serverData) period
    startServer serverData
