{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}

module Models.OAuth where

import Control.Lens ((&), (.~))
import Data.Aeson (FromJSON)
import Data.ByteString
import Data.Text (Text)
import Data.Map (Map)
import GHC.Generics (Generic)

import Network.Wreq

data OAuthRequest = OAuthRequest
  { code :: String
  , clientId :: String
  , clientSecret :: String }

data OAuthResponse = OAuthResponse
  { accessToken :: String
  , refreshToken :: String
  } deriving (Show, Generic)
instance FromJSON OAuthResponse

requestToken :: String -> Text -> IO (Response OAuthResponse)
requestToken endpoint code = do
  let opts = defaults & param code .~ [code]
  r <- asJSON =<< getWith opts endpoint
  pure r