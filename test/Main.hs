{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Aeson         as Aeson
import           Decaf.Client
import           Mocking            (MockAnythingResponseBody(..), runMockRequest, runMockRequestJson)
import           Network.HTTP.Types (Status(statusCode))
import           Test.Hspec         (Selector, describe, hspec, it, shouldReturn, shouldThrow)


main :: IO ()
main = putStrLn "Tests are disabled!"
-- main = hspec $ do
--   describe "Non-2xx Responses" $ do
--     it "404 raises exception" $ do
--       runMockRequest (path "status/404") `shouldThrow` anyDecafClientException
--   describe "Request Payload" $ do
--     it "No request payload" $ do
--       mockAnythingResponseBodyData . decafResponseBody <$> runMockRequestJson (path "anything") `shouldReturn` ""
--     it "Some plain request payload" $ do
--       mockAnythingResponseBodyData . decafResponseBody <$> runMockRequestJson (path "anything" . payload "text/plain" "Hello World") `shouldReturn` "Hello World"
--     it "Some JSON request payload" $ do
--       mockAnythingResponseBodyData . decafResponseBody <$> runMockRequestJson (path "anything" . jsonPayload (Aeson.Number 10)) `shouldReturn` "10"
--       mockAnythingResponseBodyJson . decafResponseBody <$> runMockRequestJson (path "anything" . jsonPayload (Aeson.Number 10)) `shouldReturn` Aeson.Number 10
--   describe "Pure Response Body" $ do
--     it "Response with response body is decoded correcly" $ do
--       decafResponseBody <$> runMockRequest (path "status/203") `shouldReturn` ""


-- anyDecafClientException :: Selector DecafClientException
-- anyDecafClientException = const True
