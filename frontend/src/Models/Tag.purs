module Models.Tag where

import Prelude

import Data.Argonaut.Core (caseJsonObject)
import Data.Argonaut.Decode (class DecodeJson, (.?))
import Data.Either (Either(..))

data Collection a = Collection
  { items :: Array a
  , total :: Number }

instance decodeCollection :: (DecodeJson a) => DecodeJson (Collection a) where
  decodeJson json = do
    content <- caseJsonObject (Left "unexpected error") Right json
    total <- content .? "total"
    items <- content .? "items"
    pure $ Collection { total, items }

getItems :: forall a. Collection a -> Array a
getItems (Collection collection) = collection.items

data Tag = Tag
  { id :: Number
  , title :: String
  , status :: Int }

instance decodeTag :: DecodeJson Tag where
  decodeJson json = do
    content <- caseJsonObject (Left "unexpected error") Right json
    id <- content .? "id"
    title <- content .? "title"
    pure $ Tag { id: id, title: title, status: 0 }

getId :: Tag -> Number
getId (Tag tag) = tag.id

getTitle :: Tag -> String
getTitle (Tag tag) = tag.title

type TagCollection = Collection Tag

data TagCategory = TagCategory
  { id :: Number
  , title :: String
  , tags :: TagCollection }

getCategoryId :: TagCategory -> Number
getCategoryId (TagCategory tagCategory) = tagCategory.id

getCategoryTitle :: TagCategory -> String
getCategoryTitle (TagCategory tagCategory) = tagCategory.title

getCategoryTags :: TagCategory -> TagCollection
getCategoryTags (TagCategory tagCategory) = tagCategory.tags

instance decodeTagCategory :: DecodeJson TagCategory where
  decodeJson json = do
    content <- caseJsonObject (Left "unexpected error") Right json
    id <- content .? "id"
    title <- content .? "title"
    tags <- content .? "tags"
    pure $ TagCategory { id, title, tags }

type TagCategoryCollection = Collection TagCategory
