{-# LANGUAGE TemplateHaskell #-}

module Models.Collection where

import Data.Int (Int64)
import Data.Aeson
import Data.Aeson.TH

data Collection a = Collection
  { items :: [a]
  , total :: Int64
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Collection)
