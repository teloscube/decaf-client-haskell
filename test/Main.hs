{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson         as Aeson
import qualified Decaf.Client       as DC
import           Mocking            (MockAnythingResponseBody(..), runMockRequest, runMockRequestJson)
import           Network.HTTP.Types (Status(statusCode))
import           Test.Hspec         (Selector, describe, hspec, it, shouldReturn, shouldThrow)


main :: IO ()
main = hspec $ do
  describe "Non-2xx Responses" $ do
    it "404 raises exception" $ do
      runMockRequest (DC.path "status/404") `shouldThrow` anyDecafClientException
  describe "Request Payload" $ do
    it "No request payload" $ do
      mockAnythingResponseBodyData . DC.decafResponseBody <$> runMockRequestJson (DC.path "anything") `shouldReturn` ""
    it "Some plain request payload" $ do
      mockAnythingResponseBodyData . DC.decafResponseBody <$> runMockRequestJson (DC.path "anything" . DC.payload "text/plain" "Hello World") `shouldReturn` "Hello World"
    it "Some JSON request payload" $ do
      mockAnythingResponseBodyData . DC.decafResponseBody <$> runMockRequestJson (DC.path "anything" . DC.jsonPayload (Aeson.Number 10)) `shouldReturn` "10"
      mockAnythingResponseBodyJson . DC.decafResponseBody <$> runMockRequestJson (DC.path "anything" . DC.jsonPayload (Aeson.Number 10)) `shouldReturn` Aeson.Number 10
  describe "Pure Response Body" $ do
    it "Response with response body is decoded correcly" $ do
      DC.decafResponseBody <$> runMockRequest (DC.path "status/203") `shouldReturn` ""


anyDecafClientException :: Selector DC.DecafClientException
anyDecafClientException = const True
