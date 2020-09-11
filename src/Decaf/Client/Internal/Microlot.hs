{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}

module Decaf.Client.Internal.Microlot where

import Data.Aeson                 (FromJSON, Object, Value, object, (.=))
import Data.HashMap.Strict        (empty)
import Decaf.Client.Internal.Http
       ( Authorization
       , BaseUrl
       , Body(JsonBody)
       , DecafRequest(DecafRequest)
       , Endpoint(Endpoint)
       , Method(POST)
       , prepareRequestWithBody
       , requestJson
       )


-- | Microlot client.
type MicrolotClient a = (FromJSON a) => GqlQuery -> IO a


-- | GraphQL query type.
type Gql = String


-- | GraphQL query variables type.
type GqlVariables = Object


-- | Data definition for GraphQL query.
data GqlQuery = GqlQuery Gql GqlVariables | GqlJustQuery Gql


-- | Creates a function which acts as a DECAF Microlot client.
mkMicrolotClient :: (FromJSON a) => BaseUrl -> Authorization -> MicrolotClient a
mkMicrolotClient baseUrl auth = runQuery baseRequest
  where
    baseRequest = DecafRequest baseUrl auth [] POST (Endpoint "/apis/microlot/v1/graphql")


-- | Auxiliary function which runs a GraphQL query over a 'DecafRequest'.
runQuery :: (FromJSON a) => DecafRequest -> GqlQuery -> IO a
runQuery request query = requestJson (prepareRequestWithBody $ compileGqlToBody query) request


-- | Compiles 'GqlQuery' to request body.
compileGqlToBody :: GqlQuery -> Body Value
compileGqlToBody (GqlJustQuery gql)  = compileGqlToBody (GqlQuery gql empty)
compileGqlToBody (GqlQuery gql vars) = JsonBody $ object [ "query" .= gql, "variables" .= vars]
