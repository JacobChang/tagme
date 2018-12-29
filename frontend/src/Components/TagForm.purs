module Components.TagForm where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Models.Tag (Tag)

type TagFormState =
  { submitting :: Boolean
  , title :: String
  , result :: Maybe String }

data TagFormQuery a
  = UpdateTitle String a
  | Submit a

type TagFormInput = Unit

data TagFormMessage
  = NewTag Tag

view :: forall m. H.Component HH.HTML TagFormQuery TagFormInput TagFormMessage m
view =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: TagFormState
    initialState =
      { submitting: false
      , title: ""
      , result: Nothing }

    render :: TagFormState -> H.ComponentHTML TagFormQuery
    render state =
      HH.div
        [ HP.class_ (H.ClassName "tag-form")]
        [ HH.div
          []
          [ HH.input 
            [ HP.title "title of tag"
            , HP.placeholder "title of tag"
            , HP.value state.title
            , HE.onValueChange (HE.input UpdateTitle)]]
        , HH.div
          []
          [ HH.button
            [ HE.onClick (HE.input_ Submit)]
            [ HH.text "Submit" ]
          ]
        ]

    eval :: TagFormQuery ~> H.ComponentDSL TagFormState TagFormQuery TagFormMessage m
    eval = case _ of
      UpdateTitle title next -> do
        H.modify_ (_ { title = title })
        pure next
      Submit next -> do
        pure next
