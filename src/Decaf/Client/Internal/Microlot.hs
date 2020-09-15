{-# LANGUAGE DeriveGeneric             #-}
-- | This module provides a DECAF Microlot client implementation.
--
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}

module Decaf.Client.Internal.Microlot where

import           Control.Monad.IO.Class            (MonadIO)
import           Data.Aeson
                 ( FromJSON(..)
                 , Object
                 , Options(fieldLabelModifier)
                 , ToJSON(..)
                 , Value
                 , defaultOptions
                 , genericParseJSON
                 , object
                 , (.=)
                 )
import           Data.Char                         (toLower)
import           Data.List.NonEmpty                (NonEmpty)
import qualified Data.Text                         as T
import qualified Decaf.Client.Internal.Combinators as IC
import qualified Decaf.Client.Internal.Http        as IH
import qualified Decaf.Client.Internal.Request     as IR
import qualified Decaf.Client.Internal.Types       as IT
import           Decaf.Client.Internal.Utils       (applyFirst)
import           GHC.Generics                      (Generic)


-- | DECAF Microlot API client type.
--
-- This is a _wrapper_ around 'IT.Request'.
newtype MicrolotClient = MkMicrolotClient { unMicrolotClient :: IT.Request } deriving Show


-- | Attempts to build a 'MicrolotClient' with the given DECAF deployment and credentials information.
--
-- >>> mkMicrolotClient "https://example.com" (IT.HeaderCredentials "OUCH")
-- Right (MkMicrolotClient {unMicrolotClient = Request {
--   requestHost              = "example.com"
--   requestPort              = Nothing
--   requestNamespace         = MkPath {unPath = ["apis","microlot","v1","graphql"]}
--   requestIsSecure          = True
--   requestCredentials       = <********>
--   requestUserAgent         = "DECAF API Client/0.0.0.1 (Haskell)"
--   requestHttpHeaders       = [("X-DECAF-URL","https://example.com")]
--   requestHttpMethod        = POST
--   requestHttpPath          = MkPath {unPath = []}
--   requestHttpTrailingSlash = False
--   requestHttpParams        = []
--   requestHttpPayload       = Nothing
-- }})
mkMicrolotClient :: T.Text -> IT.Credentials -> Either String MicrolotClient
mkMicrolotClient d c = MkMicrolotClient . IC.post . IC.namespace "/apis/microlot/v1/graphql" . IC.withoutTrailingSlash <$> IR.initRequest d c


-- | Runs the 'BaristaClient' along with given 'IR.Request' combinators and
-- returns a 'IV.Response' value JSON-decoded from the response body.
runMicrolot :: (MonadIO m, ToJSON a, Show b, FromJSON b) => MicrolotQuery a -> MicrolotClient-> m (IT.Response (MicrolotResponse b))
runMicrolot query cli = IH.runRequest $ mkRequest (IC.jsonPayload query) cli


-- | Microlot query type as a sealed query/variables tuple.
data MicrolotQuery a = ToJSON a => MkMicrolotQuery !String !a

instance ToJSON (MicrolotQuery a) where
  toJSON (MkMicrolotQuery q v) = object ["query" .= q, "variables" .= v]


mkMicrolotQuery :: ToJSON a => String -> a -> MicrolotQuery a
mkMicrolotQuery = MkMicrolotQuery


mkMicrolotQuery' :: String -> MicrolotQuery Value
mkMicrolotQuery' = flip MkMicrolotQuery $ object []


-- | Microlot response definition.
data MicrolotResponse a = MicrolotResponse
  { microlotResponseData   :: !a
  , microlotResponseErrors :: !(Maybe (NonEmpty Object))
  } deriving (Generic, Show)

instance (FromJSON a) => FromJSON (MicrolotResponse a) where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = applyFirst toLower . drop 16 }


--------------------
-- BEGIN INTERNAL --
--------------------


-- | Builds an 'IT.Request' from a 'MicrolotClient' while applying a 'IC.Combinator'.
mkRequest :: IC.Combinator -> MicrolotClient -> IT.Request
mkRequest c = c . unMicrolotClient


------------------
-- END INTERNAL --
------------------
