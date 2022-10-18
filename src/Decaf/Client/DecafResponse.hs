-- | This module provides definitions to work with DECAF client responses.
{-# LANGUAGE OverloadedStrings #-}

module Decaf.Client.DecafResponse where

import           Data.Aeson          ((.:))
import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.KeyMap
import           Network.HTTP.Types  (ResponseHeaders, Status)


-- | Data definition for DECAF client response values.
data DecafResponse a = DecafResponse
  { decafResponseStatus  :: !Status
  , decafResponseHeaders :: !ResponseHeaders
  , decafResponseBody    :: !a
  }
  deriving Show


-- | Data definition for DECAF GraphQL query results.
data DecafGraphqlQueryResult a =
    DecafGraphqlQueryResultSuccess !a
  | DecafGraphqlQueryResultFailure !Aeson.Value
  deriving Show


instance Aeson.FromJSON a => Aeson.FromJSON (DecafGraphqlQueryResult a) where
  parseJSON = Aeson.withObject "DecafGraphqlQueryResult" $ \o -> do
    case Data.Aeson.KeyMap.lookup "errors" o of
      Nothing -> DecafGraphqlQueryResultSuccess <$> o .: "data"
      Just va -> pure (DecafGraphqlQueryResultFailure va)
