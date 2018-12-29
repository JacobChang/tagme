module Components.CategoryForm where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type CategoryFormFields =
  { title :: String }

type CategoryFormState =
  { submitting :: Boolean
  , fields :: CategoryFormFields }

data CategoryFormQuery a
  = UpdateTitle String a
  | SubmitForm a

type CategoryFormInput = Unit

data CategoryFormMessage
  = CreateCategory CategoryFormFields

view :: forall m. H.Component HH.HTML CategoryFormQuery CategoryFormInput CategoryFormMessage m
view =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: CategoryFormState
    initialState =
      { submitting: false
      , fields: { title: "" }
      }

    render :: CategoryFormState -> H.ComponentHTML CategoryFormQuery
    render state =
      HH.form
        [ HP.class_ (H.ClassName "tag-form form form--vertical")]
        [ HH.div
          [ HP.class_ (H.ClassName "form__field")]
          [ HH.input 
            [ HP.class_ (H.ClassName "form__control")
            , HP.title "title of tag"
            , HP.placeholder "title of tag"
            , HP.value state.fields.title
            , HE.onValueChange (HE.input UpdateTitle)]]
        , HH.div
          [ HP.class_ (H.ClassName "form__submit") ]
          [ HH.button
            [ HE.onClick (HE.input_ SubmitForm)
            , HP.disabled state.submitting ]
            [ HH.text "Submit" ]
          ]
        ]

    eval :: CategoryFormQuery ~> H.ComponentDSL CategoryFormState CategoryFormQuery CategoryFormMessage m
    eval = case _ of
      UpdateTitle title next -> do
        H.modify_ (_ { fields { title = title } })
        pure next
      SubmitForm next -> do
        state <- H.get
        H.raise $ CreateCategory state.fields
        pure next
