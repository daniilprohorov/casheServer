{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Client (
    weatherRequest,
    ClientApiInfo (..),
) where

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Text           (Text, pack, unpack)
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant
import           Servant.Client

type APIClient = "weather" :> QueryParam "q" Text :> QueryParam "appid" Text :> Get '[JSON] Object

data ClientApiInfo = ClientApiInfo
    { key  :: Text
    , root :: Text
    , path :: Text
    , port :: Int
    }

apiclient :: Proxy APIClient
apiclient = Proxy

getWeather :: Maybe Text -> Maybe Text -> ClientM Object
getWeather = client apiclient

weatherRequest :: ClientApiInfo -> Text -> IO Object
weatherRequest ClientApiInfo{key, root, path, port} location = do
    -- not the best way to solve errors messages and codes,
    -- but translating errors realization is just spend much time
    let fromEither = either (\msg -> HM.singleton (pack "Error") (String $ pack $ show msg)) id
    manager <- newManager defaultManagerSettings
    let env = mkClientEnv manager $ BaseUrl Http (unpack root) port (unpack path)
    let getWeatherRequest = getWeather (Just location) (Just key)
    res <- runClientM getWeatherRequest env
    return $ fromEither res
