{-# LANGUAGE TemplateHaskell #-}

module Models.User where

import Data.Char (toLower)
import Data.Int (Int64)
import Data.UUID as UUID
import Data.Aeson
import Data.Aeson.TH
import Database.PostgreSQL.Simple

data User = User
  { userId :: Int64
  , userUuid :: UUID.UUID
  , userStatus :: Int }

$(deriveJSON defaultOptions{fieldLabelModifier = drop (length "user") . map toLower} ''User)

signin :: Connection -> IO User
signin conn = do
  return User
    { userId = 0
    , userUuid = UUID.nil
    , userStatus = 0 }

signout :: Connection -> IO User
signout conn = do
  return User
    { userId = 0
    , userUuid = UUID.nil
    , userStatus = 0 }