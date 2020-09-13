-- | This module provides a DECAF Barista API client implementation.
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}

module Decaf.Client.Internal.Barista where

import           Data.Aeson                 (FromJSON, ToJSON)
import qualified Data.ByteString            as B
import qualified Decaf.Client.Internal.Http as H


-- | DECAF Barista API client type.
newtype BaristaClient = MkBaristaClient { unBaristaClient :: H.Request }


-- | Builds a DECAF Barista API client.
mkBaristaClient :: H.Url -> H.Credentials -> BaristaClient
mkBaristaClient baseUrl credentials = MkBaristaClient request
  where
    auth = H.mkAuthorization credentials
    request = H.setEndpoint baristaEndpoint $ H.mkRequest (H.mkBaseUrl baseUrl) auth []


modifyClient :: (H.Request -> H.Request) -> BaristaClient -> BaristaClient
modifyClient modifier = MkBaristaClient . modifier . unBaristaClient


mkPath :: [String] -> H.Path
mkPath = H.mkPath True


setEndpoint :: H.Method -> [String] -> BaristaClient -> BaristaClient
setEndpoint method path = modifyClient $ H.setEndpoint (H.Endpoint method (mkPath path))


setParams :: H.Params -> BaristaClient -> BaristaClient
setParams = modifyClient . H.setParams


addParams :: H.Params -> BaristaClient -> BaristaClient
addParams = modifyClient . H.addParams


addHeaders :: H.Headers -> BaristaClient -> BaristaClient
addHeaders = modifyClient . H.addHeaders


addPath :: [String] -> BaristaClient -> BaristaClient
addPath = modifyClient . H.addPath . H.mkPath True


-- | Add 'ByteString' payload to 'Request' along with its content-type.
addPayload :: H.ContentType -> B.ByteString -> BaristaClient -> BaristaClient
addPayload ctype = modifyClient . H.addPayload ctype


-- | Add JSON payload to 'Request'.
addJsonPayload :: ToJSON a =>  a -> BaristaClient -> BaristaClient
addJsonPayload = modifyClient . H.addJsonPayload


-- | Attempts to perform a Barista request that returns 'B.ByteString'.
perform :: BaristaClient -> IO B.ByteString
perform (MkBaristaClient request) = H.perform request


-- | Attempts to perform a Barista request that returns returns a value decoded from a JSON response body.
performJson :: FromJSON a => BaristaClient -> IO a
performJson (MkBaristaClient request) = H.performJson request


-- | Prepares a get request.
prepareGet :: H.Params ->  [String] -> H.Headers -> BaristaClient -> BaristaClient
prepareGet params path headers = addParams params . setEndpoint H.GET path . addHeaders headers

-- | Attempt to perform a GET request.
get :: H.Params ->  [String] -> H.Headers -> BaristaClient -> IO B.ByteString
get params path headers = perform . prepareGet params path headers


-- | Attempt to perform a GET request that returns a value decoded from the JSON response body.
getJson :: FromJSON a => H.Params -> [String] -> H.Headers -> BaristaClient -> IO a
getJson params path headers = performJson . prepareGet params path headers


-- | Base Barista endpoint.
baristaEndpoint :: H.Endpoint
baristaEndpoint = H.Endpoint H.GET (mkPath ["api"])
