module Components.CategoryCreatePage where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Components.CategoryForm (CategoryFormMessage(..), CategoryFormQuery, CategoryFormFields)
import Components.CategoryForm as CategoryForm
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data CategoryBody = CategoryBody CategoryFormFields

instance categoryBodyEncodeJson :: EncodeJson CategoryBody where
  encodeJson (CategoryBody fields) = do
    "title" := fields.title
    ~> jsonEmptyObject

type CategoryCreatePageState =
  { title :: String }

data CategoryCreatePageQuery a
  = HandleForm CategoryFormMessage a

type ChildQuery = CategoryFormQuery

data CategoryFormSlot = CategoryFormSlot
derive instance eqCategoryFormSlot :: Eq CategoryFormSlot
derive instance ordCategoryFormSlot :: Ord CategoryFormSlot

type ChildSlot = CategoryFormSlot

type CategoryCreatePageInput = Unit

type CategoryCreatePageMessage = Void

view :: H.Component HH.HTML CategoryCreatePageQuery CategoryCreatePageInput CategoryCreatePageMessage Aff
view =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing }
  where
    initialState :: CategoryCreatePageState
    initialState =
      { title: "Create New Category" }

    render :: CategoryCreatePageState -> H.ParentHTML CategoryCreatePageQuery ChildQuery ChildSlot Aff
    render state =
      let
        tagFormSlot = HH.slot CategoryFormSlot CategoryForm.view unit (HE.input HandleForm)
      in
        HH.div
          [ HP.class_ (ClassName "category-create-page")]
          [ tagFormSlot ]

    eval :: CategoryCreatePageQuery ~> H.ParentDSL CategoryCreatePageState CategoryCreatePageQuery ChildQuery ChildSlot CategoryCreatePageMessage Aff
    eval query =
      case query of
        HandleForm (CreateCategory fields) next -> do
          response <- H.liftAff $ AX.post ResponseFormat.json "/api/v1/categories" (RequestBody.json (encodeJson (CategoryBody fields)))
          pure next
