{-# LANGUAGE TemplateHaskell #-}

module Models.Category where

import Data.Char (toLower)
import Data.Int (Int64)
import Data.Aeson
import Data.Aeson.TH
import Database.PostgreSQL.Simple
import Models.Collection
import Models.Tag

data TagCategory = TagCategory
  { tagCategoryId :: Int64
  , tagCategoryTitle :: String
  , tagCategoryTags :: Collection Tag
  , tagCategoryStatus :: Int
  } deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (length "tagCategory") . map toLower } ''TagCategory)

tagCollection = Collection
  { items = [ tag ]
  , total = 1 }

tagCategory = TagCategory
  { tagCategoryId = 0
  , tagCategoryTitle = "list 0"
  , tagCategoryTags = tagCollection
  , tagCategoryStatus = 0 }

tagCategory1 = TagCategory
  { tagCategoryId = 1
  , tagCategoryTitle = "list 0"
  , tagCategoryTags = tagCollection
  , tagCategoryStatus = 0 }

type TagCategoryCollection = IO (Collection TagCategory)

getTagCategories :: Connection -> TagCategoryCollection
getTagCategories conn = do
  return Collection
    { items = [ tagCategory, tagCategory1 ]
    , total = 2 }
