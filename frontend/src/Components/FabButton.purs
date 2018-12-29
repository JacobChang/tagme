module Components.FabButton where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Data.Maybe (Maybe(..))

type FabButtonState =
  { title :: String
  , link :: String
  , icon :: String }

data FabButtonQuery a
  = Exec a

data FabButtonMessage
  = ExecFabAction

type FabButtonInput = Unit

view :: forall m. FabButtonState -> H.Component HH.HTML FabButtonQuery FabButtonInput FabButtonMessage m
view initialState =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: FabButtonState -> H.ComponentHTML FabButtonQuery
  render state =
    HH.a
      [ HP.class_ (H.ClassName "fab-button")
      , HP.href state.link ]
      [ HH.text state.title ]

  eval :: FabButtonQuery ~> H.ComponentDSL FabButtonState FabButtonQuery FabButtonMessage m
  eval = case _ of
    Exec next -> do
      H.raise $ ExecFabAction
      pure next
