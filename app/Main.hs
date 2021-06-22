module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  [Just port, Just apiKey, Just apiRoot, Just apiPath] <- mapM lookupEnv ["PORT", "API_KEY", "API_ROOT", "API_PATH"]
  let state = State (read port :: Int) apiKey apiRoot apiPath ["London"]
  startApp state
