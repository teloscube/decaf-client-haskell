{-# LANGUAGE OverloadedStrings #-}

module Decaf.Client where

import           Data.Aeson          (ToJSON)
import           Data.Aeson.Types    (FromJSON)
import qualified Data.ByteString     as B
import           Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest_, setRequestBodyJSON, setRequestHeader)
import           Text.Printf         (printf)


-- | DECAF API Client information.
--
-- This provides the minimum required information to build various DECAF API
-- clients.
data DecafClientInfo = DecafClientInfo
  { decafClientInfoDeploymentUrl :: !String,        -- ^ Base deployment URL.
    decafClientInfoAuthorization :: !B.ByteString   -- ^ HTTP @Authorization@ header value.
  }


-- | DECAF Microlot client function.
microlot :: (ToJSON a, FromJSON b) => DecafClientInfo -> a -> IO b
microlot (DecafClientInfo url auth) variables = do
  let endpoint = printf "POST %s/apis/microlot/v1/graphql" url
  let request = requestMaker endpoint
  getResponseBody <$> httpJSON request
  where
    requestMaker = setRequestHeader "Authorization" [auth] . setRequestBodyJSON variables . parseRequest_
