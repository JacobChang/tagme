{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib (app)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do
    describe "GET /api/v1/tags" $ do
        it "responds with 200" $ do
            get "/api/v1/tags" `shouldRespondWith` 200
        it "responds with tags" $ do
            let tags = "{\"items\":[{\"id\":0,\"title\":\"list 0\",\"tags\":{\"items\":[{\"id\":0,\"title\":\"tag 0\"}],\"total\":1}}],\"total\":1}"
            get "/api/v1/tags" `shouldRespondWith` tags
