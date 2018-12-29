{-# LANGUAGE TemplateHaskell #-}

module Models.Tag where

import Data.Char (toLower)
import Data.Int (Int64)
import Data.Aeson
import Data.Aeson.TH
import Database.PostgreSQL.Simple

data Tag = Tag
  { tagId :: Int64
  , tagTitle :: String
  , tagStatus :: Int
  } deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (length "tag") . map toLower} ''Tag)

tag = Tag
  { tagId = 0
  , tagTitle = "tag 0"
  , tagStatus = 0 }