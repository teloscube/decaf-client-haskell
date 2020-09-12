{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}

module Decaf.Client.Internal.Microlot where

import Data.Aeson                 (FromJSON, ToJSON(..), object, (.=))
import Decaf.Client.Internal.Http
       ( Authorization
       , BaseUrl
       , DecafRequest
       , Endpoint(Endpoint)
       , Method(POST)
       , Path(..)
       , addJsonBody
       , mkDecafRequest
       , performRequestJson
       , setEndpoint
       )


-- | DECAF Microlot API client type.
newtype MicrolotClient = MkMicrolotClient DecafRequest


-- | Builds a DECAF Microlot API client.
mkMicrolotClient :: BaseUrl -> Authorization -> MicrolotClient
mkMicrolotClient baseUrl auth = MkMicrolotClient $ setEndpoint microlotEndpoint $ mkDecafRequest baseUrl auth []


-- | Microlot request type.
data MicrolotRequest a = ToJSON a => MkMicrolotRequest (String, a)

instance ToJSON (MicrolotRequest a) where
  toJSON (MkMicrolotRequest (gql, vars)) = object ["query" .= gql, "variables" .= vars]


-- | Performs a DECAF Microlot API request.
runMicrolot :: (ToJSON a, FromJSON b) => MicrolotClient -> MicrolotRequest a -> IO b
runMicrolot (MkMicrolotClient decafRequest) request = performRequestJson $ addJsonBody request decafRequest


-- | DECAF Microlot API endpoint.
microlotEndpoint :: Endpoint
microlotEndpoint = Endpoint POST $ Path "/apis/microlot/v1/graphql"
