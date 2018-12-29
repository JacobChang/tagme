module Components.CategoriesPage where

import Prelude

import Components.FabButton (FabButtonMessage(..), FabButtonQuery)
import Components.FabButton as FabButton
import Components.TagCategory (TagCategoryMessage(..), TagCategoryQuery)
import Components.TagCategory as TagCategory
import Components.TagSearch (TagSearchMessage(..), TagSearchQuery)
import Components.TagSearch as TagSearch
import Data.Array (cons, snoc)
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Effect.Aff (Aff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Models.Tag (TagCategoryCollection)
import Models.Tag as TagModel
import Web.Event.Event (preventDefault)

type CategoriesPageState =
  { fabButton :: FabButton.FabButtonState
  , tagCategories :: TagModel.TagCategoryCollection }

data CategoriesPageQuery a
  = UpdateCategory TagModel.TagCategoryCollection a
  | HandleCategory TagCategoryMessage a
  | HandleSearch TagSearchMessage a
  | HandleFabButton FabButtonMessage a

type ChildQuery = Coproduct3 TagCategoryQuery TagSearchQuery FabButtonQuery

data TagCategorySlot = TagCategorySlot Number
derive instance eqTagCategorySlot :: Eq TagCategorySlot
derive instance ordTagCategorySlot :: Ord TagCategorySlot

data TagSearchSlot = TagSearchSlot
derive instance eqTagSearchSlot :: Eq TagSearchSlot
derive instance ordTagSearchSlot :: Ord TagSearchSlot

data FabButtonSlot = FabButtonSlot
derive instance eqFabButtonSlot :: Eq FabButtonSlot
derive instance ordFabButtonSlot :: Ord FabButtonSlot

type ChildSlot = Either3 TagCategorySlot TagSearchSlot FabButtonSlot

type CategoriesPageInput = TagModel.TagCategoryCollection

type CategoriesPageMessage = Void

view :: TagCategoryCollection -> H.Component HH.HTML CategoriesPageQuery CategoriesPageInput CategoriesPageMessage Aff
view tagCategories =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: HE.input UpdateCategory }
  where
    initialState :: CategoriesPageState
    initialState =
      { fabButton: { title: "Create", icon: "create", link: "/#/categories/create" }
      , tagCategories: tagCategories }

    render :: CategoriesPageState -> H.ParentHTML CategoriesPageQuery ChildQuery ChildSlot Aff
    render state =
      let
        tagCategorySlot tagCategory = HH.slot' CP.cp1 (TagCategorySlot (TagModel.getCategoryId tagCategory)) (TagCategory.view tagCategory) tagCategory (HE.input HandleCategory)
        tagCategorySlots = map tagCategorySlot (TagModel.getItems state.tagCategories)
        tagSearchSlot = HH.slot' CP.cp2 TagSearchSlot TagSearch.view unit (HE.input HandleSearch)
        fabButtonSlot = HH.slot' CP.cp3 FabButtonSlot (FabButton.view state.fabButton) unit (HE.input HandleFabButton)
      in
        HH.div
          [ HP.class_ (ClassName "CategoriesPage")]
          (snoc (cons tagSearchSlot tagCategorySlots) fabButtonSlot)

    eval :: CategoriesPageQuery ~> H.ParentDSL CategoriesPageState CategoriesPageQuery ChildQuery ChildSlot CategoriesPageMessage Aff
    eval query =
      case query of
        UpdateCategory newTagCategories next -> do
          H.modify_ (_ { tagCategories = newTagCategories })
          pure next
        HandleCategory (CategoryTagCreate) next -> do
          pure next
        HandleCategory (CategoryTagSelect tag) next -> do
          pure next
        HandleCategory (CategoryTagRemove tag) next -> do
          pure next
        HandleSearch (SearchTags evt keyword) next -> do
          H.liftEffect $ preventDefault evt
          pure next
        HandleFabButton ExecFabAction next -> do
          pure next
