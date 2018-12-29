module Components.Tag where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Models.Tag as TagModel

type TagState =
  { tag :: TagModel.Tag }

data TagQuery a
  = Select a
  | Update TagModel.Tag a
  | Remove a

type TagInput = TagModel.Tag

data TagMessage
  = TagSelect TagModel.Tag
  | TagRemove TagModel.Tag

view :: forall m. TagModel.Tag -> H.Component HH.HTML TagQuery TagInput TagMessage m
view initTag =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: HE.input Update
    }
  where

  initialState :: TagState
  initialState =
    { tag : initTag }

  render :: TagState -> H.ComponentHTML TagQuery
  render state =
    HH.div
      [ HP.class_ (H.ClassName "tag")
      , HE.onClick (HE.input_ Select)
      ]
      [ HH.text (TagModel.getTitle state.tag) ]

  eval :: TagQuery ~> H.ComponentDSL TagState TagQuery TagMessage m
  eval = case _ of
    Select next -> do
      state <- H.get
      H.raise $ TagSelect state.tag
      pure next
    Remove next -> do
      state <- H.get
      H.raise $ TagRemove state.tag
      pure next
    Update tag next -> do
      H.put { tag }
      pure next