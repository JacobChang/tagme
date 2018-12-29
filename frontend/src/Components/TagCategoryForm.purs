module Components.TagCategoryForm where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Models.Tag (TagCategory)


type TagCategoryFormState =
  { submitting :: Boolean
  , title :: String
  , result :: Maybe String }

data TagCategoryFormQuery a
  = UpdateTitle String a
  | Submit a

type TagCategoryFormInput = Unit

data TagCategoryFormMessage
  = NewTagCategory TagCategory

view :: forall m. H.Component HH.HTML TagCategoryFormQuery TagCategoryFormInput TagCategoryFormMessage m
view =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: TagCategoryFormState
    initialState =
      { submitting: false
      , title: ""
      , result: Nothing }

    render :: TagCategoryFormState -> H.ComponentHTML TagCategoryFormQuery
    render state =
      HH.div
        [ HP.class_ (H.ClassName "tag-category-form")]
        [ HH.div
          []
          [ HH.input 
            [ HP.title "title of tag category"
            , HP.placeholder "title of tag category"
            , HP.value state.title
            , HE.onValueChange (HE.input UpdateTitle)]]
        , HH.div
          []
          [ HH.button
            [ HE.onClick (HE.input_ Submit)]
            [ HH.text "Submit" ]
          ]
        ]

    eval :: TagCategoryFormQuery ~> H.ComponentDSL TagCategoryFormState TagCategoryFormQuery TagCategoryFormMessage m
    eval = case _ of
      UpdateTitle title next -> do
        H.modify_ (_ { title = title })
        pure next
      Submit next -> do
        pure next