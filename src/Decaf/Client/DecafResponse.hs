-- | This module provides definitions to work with DECAF client responses.
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Decaf.Client.DecafResponse where

import           Data.Aeson          ((.:))
import qualified Data.Aeson          as Aeson
import           Network.HTTP.Types  (ResponseHeaders, Status)

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap
#else
import qualified Data.HashMap.Strict as HM
#endif


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
#if MIN_VERSION_aeson(2,0,0)
    let merrs = Data.Aeson.KeyMap.lookup "errors" o
#else
    let merrs = HM.lookup "errors" o
#endif
    case merrs of
      Nothing -> DecafGraphqlQueryResultSuccess <$> o .: "data"
      Just va -> pure (DecafGraphqlQueryResultFailure va)
