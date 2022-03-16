-- | This module provides a DECAF Microlot client implementation.

module Decaf.Client.Internal.Apis.Microlot where

import           Control.Monad.Except          (MonadError)
import           Control.Monad.IO.Class        (MonadIO)
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
import           Data.Char                     (toLower)
import           Data.List.NonEmpty            (NonEmpty)
import qualified Data.Text                     as T
import           Decaf.Client.DecafCredentials (DecafCredentials)
import           Decaf.Client.DecafRemote      (DecafRemote, parseRemote)
import           Decaf.Client.DecafRequest
                 ( DecafRequest
                 , DecafRequestCombinator
                 , initRequest
                 , jsonPayload
                 , namespace
                 , post
                 , withoutTrailingSlash
                 )
import           Decaf.Client.DecafResponse    (DecafResponse)
import           Decaf.Client.Internal.Error   (DecafClientError)
import           Decaf.Client.Internal.Http    (runRequest)
import           Decaf.Client.Internal.Utils   (applyFirst)
import           GHC.Generics                  (Generic)


-- * Data Definition
-- $dataDefinition


-- | DECAF Microlot API client type.
--
-- This is a /wrapper/ around 'DecafRequest'.
newtype MicrolotClient = MkMicrolotClient { unMicrolotClient :: DecafRequest } deriving Show


-- | Builds a 'MicrolotClient' with the given DECAF Instance 'Remote' and
-- credentials.
--
-- >>> import Decaf.Client.DecafCredentials
-- >>> import Decaf.Client.DecafRemote
-- >>> mkMicrolotClient (Remote "example.com" Nothing True) (CredentialsHeader "OUCH") :: MicrolotClient
-- MkMicrolotClient {unMicrolotClient = Request {
--   requestRemote            = [https]://[example.com]:[443]
--   requestNamespace         = MkDecafRequestPath {unDecafRequestPath = ["apis","microlot","v1","graphql"]}
--   requestCredentials       = <********>
--   requestUserAgent         = "DECAF API Client/... (Haskell)"
--   requestHttpHeaders       = [("X-DECAF-URL","https://example.com:443")]
--   requestHttpMethod        = POST
--   requestHttpPath          = MkDecafRequestPath {unDecafRequestPath = []}
--   requestHttpTrailingSlash = False
--   requestHttpQuery         = []
--   requestHttpPayload       = Nothing
-- }}
mkMicrolotClient :: DecafRemote -> DecafCredentials -> MicrolotClient
mkMicrolotClient r c = MkMicrolotClient . post . namespace "/apis/microlot/v1/graphql" . withoutTrailingSlash $ initRequest r c


-- * Constructors
-- $constructors


-- | Attempts to build a 'MicrolotClient' with the given DECAF Instance URL and
-- credentials.
--
-- >>> import Decaf.Client.DecafCredentials
-- >>> import Decaf.Client.DecafRemote
-- >>> mkMicrolotClientM "https://example.com" (CredentialsHeader "OUCH") :: Either DecafClientError MicrolotClient
-- Right (MkMicrolotClient {unMicrolotClient = Request {
--   requestRemote            = [https]://[example.com]:[443]
--   requestNamespace         = MkDecafRequestPath {unDecafRequestPath = ["apis","microlot","v1","graphql"]}
--   requestCredentials       = <********>
--   requestUserAgent         = "DECAF API Client/... (Haskell)"
--   requestHttpHeaders       = [("X-DECAF-URL","https://example.com:443")]
--   requestHttpMethod        = POST
--   requestHttpPath          = MkDecafRequestPath {unDecafRequestPath = []}
--   requestHttpTrailingSlash = False
--   requestHttpQuery         = []
--   requestHttpPayload       = Nothing
-- }})
mkMicrolotClientM :: MonadError DecafClientError m => T.Text -> DecafCredentials -> m MicrolotClient
mkMicrolotClientM d c = (`mkMicrolotClient` c) <$> parseRemote d


-- * Queries
-- $queries


-- | Microlot query type as a sealed query/variables tuple.
data MicrolotQuery a = ToJSON a => MkMicrolotQuery !String !a


instance ToJSON (MicrolotQuery a) where
  toJSON (MkMicrolotQuery q v) = object ["query" .= q, "variables" .= v]


-- | Builds a 'MicrolotQuery' with the given GraphQL query and GraphQL query
-- variables.
mkMicrolotQuery :: ToJSON a => String -> a -> MicrolotQuery a
mkMicrolotQuery = MkMicrolotQuery


-- | Builds a 'MicrolotQuery' with the given GraphQL query (without GraphQL
-- query variables).
mkMicrolotQuery' :: String -> MicrolotQuery Value
mkMicrolotQuery' = flip MkMicrolotQuery $ object []


-- * Response
-- $response


-- | Microlot response definition.
data MicrolotResponse a = MicrolotResponse
  { microlotResponseData   :: !a
  , microlotResponseErrors :: !(Maybe (NonEmpty Object))
  } deriving (Generic, Show)


instance (FromJSON a) => FromJSON (MicrolotResponse a) where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = applyFirst toLower . drop 16 }


-- * Runners
-- $runners


-- | Runs the 'MicrolotClient' along with given 'DecafRequest' combinators and
-- returns a 'Response' value JSON-decoded from the response body.
runMicrolot :: (MonadIO m, ToJSON a, Show b, FromJSON b) => MicrolotQuery a -> MicrolotClient-> m (DecafResponse (MicrolotResponse b))
runMicrolot query cli = runRequest $ mkRequest (jsonPayload query) cli


-- * Internal
-- $internal


-- | Builds an 'DecafRequest' from a 'MicrolotClient' while applying a 'Combinator'.
mkRequest :: DecafRequestCombinator -> MicrolotClient -> DecafRequest
mkRequest c = c . unMicrolotClient
