module Main where

import Lib

testAppState = State
  { port = 8080
  , apiKey = ""
  , apiRoot = "api.openweathermap.org"
  , apiPath = "/data/2.5/"
  , locations = ["London"]
  }

main :: IO ()
main = do
  let apiRoot = "http://api.openweathermap.org/data/2.5/"
  startApp testAppState
