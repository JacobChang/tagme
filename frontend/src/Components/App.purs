module Components.App where

import Prelude

import Components.CategoriesPage (CategoriesPageQuery)
import Components.CategoriesPage as CategoriesPage
import Components.CategoryCreatePage (CategoryCreatePageQuery)
import Components.CategoryCreatePage as CategoryCreatePage
import Components.TagCreatePage (TagCreatePageQuery)
import Components.TagCreatePage as TagCreatePage
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Models.Tag as TagModel
import Router (AppRoute(..), routeName)

type AppState =
  { activeRoute :: Maybe AppRoute
  , tagCategories :: TagModel.TagCategoryCollection }

data AppQuery a
  = Route (Maybe AppRoute) a
  | UpdateCategory TagModel.TagCategoryCollection a

type ChildQuery = Coproduct3 CategoriesPageQuery CategoryCreatePageQuery TagCreatePageQuery

data CategoriesPageSlot = CategoriesPageSlot
derive instance eqCategoriesPageSlot :: Eq CategoriesPageSlot
derive instance ordCategoriesPageSlot :: Ord CategoriesPageSlot

data CategoryCreatePageSlot = CategoryCreatePageSlot
derive instance eqCategoryCreatePageSlot :: Eq CategoryCreatePageSlot
derive instance ordCategoryCreatePageSlot :: Ord CategoryCreatePageSlot

data TagCreatePageSlot = TagCreatePageSlot
derive instance eqTagCreatePageSlot :: Eq TagCreatePageSlot
derive instance ordTagCreatePageSlot :: Ord TagCreatePageSlot

type ChildSlot = Either3 CategoriesPageSlot CategoryCreatePageSlot TagCreatePageSlot

type AppInput = TagModel.TagCategoryCollection

type AppMessage = Void

view :: H.Component HH.HTML AppQuery AppInput AppMessage Aff
view =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: HE.input UpdateCategory }
  where
    initialState :: AppState
    initialState =
      { activeRoute: Just Home
      , tagCategories: TagModel.Collection { total: toNumber 0, items: []} }

    render :: AppState -> H.ParentHTML AppQuery ChildQuery ChildSlot Aff
    render state =
      let
        page = case state.activeRoute of
          Just CreateCategory ->
            HH.slot' CP.cp2 CategoryCreatePageSlot CategoryCreatePage.view unit absurd
          Just (CreateTag categoryId) ->
            HH.slot' CP.cp3 TagCreatePageSlot TagCreatePage.view unit absurd
          _ ->
            HH.slot' CP.cp1 CategoriesPageSlot (CategoriesPage.view state.tagCategories) state.tagCategories absurd           
      in
        HH.div
          [ HP.class_ (ClassName "app")]
          [ page ]

    eval :: AppQuery ~> H.ParentDSL AppState AppQuery ChildQuery ChildSlot AppMessage Aff
    eval query =
      case query of
        Route route next -> do
          H.liftEffect $ log (routeName route)
          H.modify_ (_ { activeRoute = route })
          pure next
        UpdateCategory tagCategories next -> do
          H.modify_ (_ { tagCategories = tagCategories })
          pure next
