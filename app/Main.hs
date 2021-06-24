{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Lib
import System.Environment
import System.Cron
import Data.Aeson
import Data.Text (Text, pack)
import Control.Concurrent.MVar
import Data.List (foldl')
import Data.Time
import qualified Data.Yaml as Y
import GHC.Generics

import qualified Data.HashMap.Strict as HM

import qualified Data.ByteString.Char8 as BS
import System.Console.CmdArgs


data Config = Config
  { port :: Int
  , apiKey :: Text
  , apiRoot :: Text
  , apiPath :: Text
  , locations :: [Text]
  , period :: Text
  } deriving (Show, Generic)


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
    configBS <- BS.readFile filepath
    let parsedContent = Y.decode configBS :: Maybe Config
    case parsedContent of
        Nothing -> error "Could not parse config file."
        (Just Config{port, apiKey, apiRoot, apiPath, locations, period}) -> do
            storage <- initStorage locations
            let state = State apiKey apiRoot apiPath storage
            tids <- execSchedule $ do
                addJob (updateStorage state) period
            startApp state port
            print tids

initStorage :: [Text] -> IO (MVar Object)
initStorage locations = newMVar $ HM.fromList $ zip locations $ repeat Null

updateStorage :: ApplicationState -> IO ()
updateStorage state@State{storage} = do
  storage_ <- takeMVar storage
  let locations = HM.keys storage_
  objects <- mapM (weatherRequest state) locations
  let newStorage = HM.fromList $ zip locations (map Object objects)
  timeDate <- getZonedTime
  putStrLn $ "Cashe server update " <> show timeDate
  putMVar storage newStorage
