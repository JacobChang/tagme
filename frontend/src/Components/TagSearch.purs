
module Components.TagSearch where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type TagSearchState =
  { keyword :: String }

data TagSearchQuery a
  = UpdateKeyword String a
  | Search Event a

type TagSearchInput = Unit

data TagSearchMessage = SearchTags Event String

view :: forall m. H.Component HH.HTML TagSearchQuery TagSearchInput TagSearchMessage m
view =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: TagSearchState
  initialState =
    { keyword : "" }

  render :: TagSearchState -> H.ComponentHTML TagSearchQuery
  render state =
    HH.form
      [ HP.class_ (H.ClassName "form form--horizontal")
      , HE.onSubmit (HE.input Search)
      ]
      [ HH.input [HP.class_ (H.ClassName "form__control"), HP.value state.keyword, HE.onValueChange (HE.input UpdateKeyword) ]
      , HH.button [HP.class_ (H.ClassName "form__submit")] [ HH.text "Search" ]
      ]

  eval :: TagSearchQuery ~> H.ComponentDSL TagSearchState TagSearchQuery TagSearchMessage m
  eval = case _ of
    UpdateKeyword keyword next -> do
      H.modify_ (_ { keyword = keyword })
      pure next
    Search evt next -> do
      state <- H.get
      H.raise $ SearchTags evt state.keyword
      pure next
