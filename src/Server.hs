{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Server (
    startServer,
    initStorage,
    updateStorage,
    ServerData (..),
) where

import           Control.Concurrent.MVar
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.HashMap.Strict      as HM
import           Data.Text                (Text)
import           Data.Time
import           Network.Wai              hiding (Response)
import           Network.Wai.Handler.Warp
import           Servant

-- may be will be better to add appid field here, but now it is read from config
type API = "weather" :> QueryParam "q" Text :> Get '[JSON] Object

api :: Proxy API
api = Proxy

data ServerData = ServerData
    { port     :: Int
    , apiFunc  :: Text -> IO Object
    , storageP :: MVar Object
    }

startServer :: ServerData -> IO ()
startServer serverData@ServerData{port} = run port $ app serverData

app :: ServerData -> Application
app serverData = serve api $ controller serverData

controller :: ServerData -> Maybe Text -> Handler Object
controller ServerData{apiFunc, storageP} (Just location) = do
    storage <- liftIO $ readMVar storageP
    case storage HM.!? location of
        Just (Object value) -> do
            liftIO $ print $ "Read from cashe. Location = " <> location
            return value
        _ -> do
            liftIO $ print $ "Make request. Location = " <> location
            liftIO $ apiFunc location
controller _ Nothing = throwError $ err404{errBody = "Please enter q=LOCATION query param"}

initStorage :: [Text] -> IO (MVar Object)
initStorage locations = newMVar $ HM.fromList $ zip locations $ repeat Null

updateStorage :: ServerData -> IO ()
updateStorage ServerData{apiFunc, storageP} = do
    storage <- takeMVar storageP
    let locations = HM.keys storage
    objects <- mapM apiFunc locations
    let newStorage = HM.fromList $ zip locations $ map Object objects
    timeDate <- getZonedTime
    putStrLn $ "Cashe server update " <> show timeDate
    putMVar storageP newStorage
