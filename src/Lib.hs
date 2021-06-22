{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE NamedFieldPuns  #-}
module Lib
    ( startApp
    , app
    , ApplicationState(..)
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai hiding (Response)
import Network.Wai.Handler.Warp
import Servant
-- import Network.HTTP.Client (newManager, defaultManagerSettings)
-- import Servant.API
--import Servant.Client
import qualified Data.HashMap.Strict as HM
import Data.Aeson
import Data.Proxy
import GHC.Generics
import Servant.Client
import Control.Monad.IO.Class

import Network.HTTP.Client (newManager, defaultManagerSettings)
import Data.Either (either)
import Data.Text (pack)

import Control.Concurrent.MVar

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

--data Body = Body {responseBody :: String} deriving (Show, Generic)

$(deriveJSON defaultOptions ''User)

apiPort = 80

type API = "weather" :> Get '[JSON] Object
type APIClient = "weather" :> QueryParam "q" String :> QueryParam "appid" String :> Get '[JSON] (Object)

data ApplicationState = State
  { port :: Int
  , apiKey :: String
  , apiRoot :: String
  , apiPath :: String
  , locations :: [String]
  }


startApp :: ApplicationState -> IO ()
startApp state@State{port} = run port (app state)

app :: ApplicationState -> Application
app state = serve api (server state)

api :: Proxy API
api = Proxy

apiclient :: Proxy APIClient
apiclient = Proxy

server :: ApplicationState -> Server API
server state = do
  res <- liftIO $ weatherRequest state
  let fromEither = either (\x -> HM.singleton (pack "error") (String $ pack $ show x)) id
  return $ fromEither res

weatherRequest state@State{apiKey, apiRoot, apiPath, locations} = do
  let location = head locations
  manager <- newManager defaultManagerSettings
  res <- runClientM (getWeather (Just location) (Just apiKey)) (mkClientEnv manager (BaseUrl Http apiRoot 80 apiPath))
  return res

getWeather :: Maybe String -> Maybe String -> ClientM (Object)
getWeather = client apiclient


users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
