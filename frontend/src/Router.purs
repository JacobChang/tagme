module Router where

import Prelude

import Control.Alternative ((<|>))
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Routing.Match (Match, root, lit, num, end)

data AppRoute
  = Home
  | Categories
  | Category Number
  | CreateCategory
  | CreateTag Number

routeName :: Maybe AppRoute -> String
routeName = case _ of
  Just Home -> "home"
  Just Categories -> "categories"
  Just (Category num) -> "category"
  Just CreateCategory -> "create category"
  Just (CreateTag num) -> "create tag"
  Nothing -> "nothing"

home :: Match AppRoute
home = Home <$ root

categories :: Match AppRoute
categories = Categories <$ (lit "" *> lit "categories" <* end)

category :: Match AppRoute
category = Category <$> (lit "" *> lit "categories" *> num <* end)

createCategory :: Match AppRoute
createCategory = CreateCategory <$ (lit "" *> lit "categories" *> lit "create")

createTag :: Match AppRoute
createTag = CreateTag <$> (lit "" *> lit "categories" *> num <* lit "create")

appRoute :: Match (Maybe AppRoute)
appRoute = oneOf
  [ Just <$> (categories <|> category <|> createCategory <|> createTag <|> home)
  , pure Nothing ]
