module Components.TagCreatePage where

import Prelude

import Components.TagForm (TagFormMessage(..), TagFormQuery)
import Components.TagForm as TagForm
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type TagCreatePageState =
  { title :: String }

data TagCreatePageQuery a
  = HandleForm TagFormMessage a

type ChildQuery = TagFormQuery

data TagFormSlot = TagFormSlot
derive instance eqTagFormSlot :: Eq TagFormSlot
derive instance ordTagFormSlot :: Ord TagFormSlot

type ChildSlot = TagFormSlot

type TagCreatePageInput = Unit

type TagCreatePageMessage = Void

view :: H.Component HH.HTML TagCreatePageQuery TagCreatePageInput TagCreatePageMessage Aff
view =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing }
  where
    initialState :: TagCreatePageState
    initialState =
      { title: "Create New Category" }

    render :: TagCreatePageState -> H.ParentHTML TagCreatePageQuery ChildQuery ChildSlot Aff
    render state =
      let
        tagFormSlot = HH.slot TagFormSlot TagForm.view unit (HE.input HandleForm)
      in
        HH.div
          [ HP.class_ (ClassName "tag-create-page")]
          [ tagFormSlot ]

    eval :: TagCreatePageQuery ~> H.ParentDSL TagCreatePageState TagCreatePageQuery ChildQuery ChildSlot TagCreatePageMessage Aff
    eval query =
      case query of
        HandleForm (NewTag tag) next -> do
          pure next
