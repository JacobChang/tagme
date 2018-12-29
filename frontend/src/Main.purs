module Main where

import Prelude

import Affjax as Affjax
import Affjax.ResponseFormat as ResponseFormat
import Components.App (AppQuery(..))
import Components.App as App
import Data.Argonaut.Decode (decodeJson)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Int (toNumber)
import Effect (Effect)
import Effect.Aff (Aff, forkAff, launchAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Models.Tag as TagModel
import Router (appRoute)
import Routing.Hash (matches)

defaultAppState :: TagModel.TagCategoryCollection
defaultAppState = TagModel.Collection
  { total: toNumber 0
  , items: [] }

route :: H.HalogenIO AppQuery Void Aff -> Aff (Effect Unit)
route io = H.liftEffect do
  matches appRoute hashChanged
  where
    hashChanged _ newRoute = do
      _ <- launchAff $ io.query $ H.action $ Route newRoute
      pure unit

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI App.view defaultAppState body
  _ <- forkAff $ route io
  response <- H.liftAff $ Affjax.request (Affjax.defaultRequest { url = "http://tagme.feblr.org/api/v1/categories", method = Left GET, responseFormat = ResponseFormat.json})
  case response.body of
    Right json ->
      case decodeJson json of
        Right categories ->
          io.query $ H.action $ App.UpdateCategory categories
        Left err ->
          log "Unexpected json error"
    Left err ->
      log "Unexpected response format"
