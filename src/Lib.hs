{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib
    ( startApp
    , app
    , ApplicationState(..)
    , weatherRequest
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai hiding (Response)
import Network.Wai.Handler.Warp
import Servant
import qualified Data.HashMap.Strict as HM
import Data.Aeson
import Data.Proxy
import GHC.Generics
import Servant.Client
import Control.Monad.IO.Class

import Network.HTTP.Client (newManager, defaultManagerSettings)
import Data.Either (either)
import Data.Text (pack, unpack)
import Data.Text (Text(..))

import Control.Concurrent.MVar

apiPort = 80
-- логично добавить appid в параметры запроса, тогда не надо будет ничего сохранять на уровне кеш сервера
type API = "weather" :> QueryParam "q" Text :> Get '[JSON] Object
type APIClient = "weather" :> QueryParam "q" Text :> QueryParam "appid" Text :> Get '[JSON] (Object)

data ApplicationState = State
  { port :: Int
  , apiKey :: Text
  , apiRoot :: Text
  , apiPath :: Text
  , storage :: MVar Object
  }


startApp :: ApplicationState -> IO ()
startApp state@State{port} = run port (app state)

app :: ApplicationState -> Application
app state = serve api (server state)

api :: Proxy API
api = Proxy

apiclient :: Proxy APIClient
apiclient = Proxy

controller :: ApplicationState -> Maybe Text -> Handler Object
controller state@State{storage} (Just location) = do
  storage_ <- liftIO $ readMVar storage
  case storage_ HM.!? location of
      Just (Object value) -> do
        liftIO $ print $ "Read from cashe. Location = " <> location
        return value
      _ -> do
        liftIO $ print $ "Make request. Location = " <> location
        res <- liftIO $ weatherRequest state location
        return res

controller _ Nothing = throwError $ err404 { errBody = "Please enter q=LOCATION query param" }

server :: ApplicationState -> Server API
server state = controller state

weatherRequest :: ApplicationState -> Text -> IO (Object)
weatherRequest state@State{apiKey, apiRoot, apiPath} location = do
  -- тут сделано по колхозному!!
  let fromEither = either (\x -> HM.singleton (pack "Error") (String $ pack $ show x)) id
  manager <- newManager defaultManagerSettings
  res <- runClientM (getWeather (Just location) (Just apiKey)) (mkClientEnv manager (BaseUrl Http (unpack apiRoot) 80 (unpack apiPath)))
  return $ fromEither res

getWeather :: Maybe Text -> Maybe Text -> ClientM (Object)
getWeather = client apiclient
