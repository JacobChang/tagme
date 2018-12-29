{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Pool
import Database.PostgreSQL.Simple
import Servant
import Data.ByteString (ByteString)
import Control.Monad.IO.Class
import Models.Collection (Collection)
import Models.Category (TagCategory, getTagCategories)
import Models.User (User, signin, signout)

type ResourceAPI = "signin" :> Get '[JSON] User
  :<|> "signout" :> Get '[JSON] User
  :<|> "categories" :> Get '[JSON] (Collection TagCategory)

type API = "api" :> "v1" :> ResourceAPI

initConnPool :: ByteString -> IO (Pool Connection)
initConnPool connStr =
  createPool (connectPostgreSQL connStr) close 2 60 10

connStr :: ByteString
connStr = "postgres://feblr:feblr@localhost/feblr"

startApp :: IO ()
startApp = do
  pool <- initConnPool connStr
  runSettings (setHost "*" $ setPort 6061 defaultSettings) (app pool)

app :: Pool Connection -> Application
app pool = serve api $ server pool

api :: Proxy API
api = Proxy

server :: Pool Connection -> Server API
server pool = callWithConn signin
  :<|> callWithConn signout
  :<|> callWithConn getTagCategories
  where
    callWithConn handler = do
      liftIO . withResource pool $ handler
