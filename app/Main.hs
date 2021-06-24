{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Lib
import System.Environment
import System.Cron
import Data.Aeson
import Data.Text (Text, pack)
import Control.Concurrent.MVar
import Data.List (foldl')
import Data.Time

import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
    storage <- initStorage ["London", "Moscow"]
    state <- getState storage
    tids <- execSchedule $ do
        addJob (updateStorage state) "* * * * *"
    startApp state
    print tids

getState storage = do
  [Just port, Just apiKey, Just apiRoot, Just apiPath] <- mapM lookupEnv ["PORT", "API_KEY", "API_ROOT", "API_PATH"]
  let state = State (read port :: Int) (pack apiKey) (pack apiRoot) (pack apiPath) storage
  return state

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
