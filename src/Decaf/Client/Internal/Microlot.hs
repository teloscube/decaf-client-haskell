-- | This module provides a DECAF Microlot client implementation.
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}

module Decaf.Client.Internal.Microlot where

import Data.Aeson                 (FromJSON, ToJSON(..), object, (.=))
import Decaf.Client.Internal.Http
       ( BaseUrl
       , Credentials
       , Endpoint(..)
       , Method(POST)
       , Request
       , addJsonPayload
       , mkAuthorization
       , mkPath
       , mkRequest
       , performJson
       , setEndpoint
       )


-- | DECAF Microlot API client type.
newtype MicrolotClient = MkMicrolotClient Request


-- | Builds a DECAF Microlot API client.
mkMicrolotClient :: BaseUrl -> Credentials -> MicrolotClient
mkMicrolotClient deployment credentials = MkMicrolotClient request
  where
    auth = mkAuthorization credentials
    request = setEndpoint microlotEndpoint $ mkRequest deployment auth []


-- | Microlot request type.
data MicrolotRequest a = ToJSON a => MkMicrolotRequest (String, a)

instance ToJSON (MicrolotRequest a) where
  toJSON (MkMicrolotRequest (gql, vars)) = object ["query" .= gql, "variables" .= vars]


-- | Performs a DECAF Microlot API request.
runMicrolot :: (ToJSON a, FromJSON b) => MicrolotClient -> MicrolotRequest a -> IO b
runMicrolot (MkMicrolotClient client) request = performJson $ addJsonPayload request client


-- | DECAF Microlot API endpoint.
microlotEndpoint :: Endpoint
microlotEndpoint = Endpoint POST $ mkPath False ["/apis/microlot/v1/graphql"]
