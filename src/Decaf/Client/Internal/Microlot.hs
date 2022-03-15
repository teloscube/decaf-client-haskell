-- | This module provides a DECAF Microlot client implementation.

module Decaf.Client.Internal.Microlot where

import           Control.Monad.Except              (MonadError)
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
import           Decaf.Client.Internal.Combinators
import           Decaf.Client.Internal.Http
import           Decaf.Client.Internal.Remote
import           Decaf.Client.Internal.Request
import           Decaf.Client.Internal.Types
import           Decaf.Client.Internal.Utils       (applyFirst)
import           GHC.Generics                      (Generic)


-- * Data Definition
-- $dataDefinition


-- | DECAF Microlot API client type.
--
-- This is a /wrapper/ around 'Request'.
newtype MicrolotClient = MkMicrolotClient { unMicrolotClient :: Request } deriving Show


-- | Builds a 'MicrolotClient' with the given DECAF Instance 'Remote' and
-- credentials.
--
-- >>> import Decaf.Client.Internal.Types
-- >>> mkMicrolotClient (Remote "example.com" Nothing True) (CredentialsHeader "OUCH") :: MicrolotClient
-- MkMicrolotClient {unMicrolotClient = Request {
--   requestRemote            = [https]://[example.com]:[443]
--   requestNamespace         = MkPath {unPath = ["apis","microlot","v1","graphql"]}
--   requestCredentials       = <********>
--   requestUserAgent         = "DECAF API Client/... (Haskell)"
--   requestHttpHeaders       = [("X-DECAF-URL","https://example.com:443")]
--   requestHttpMethod        = POST
--   requestHttpPath          = MkPath {unPath = []}
--   requestHttpTrailingSlash = False
--   requestHttpParams        = []
--   requestHttpPayload       = Nothing
-- }}
mkMicrolotClient :: Remote -> Credentials -> MicrolotClient
mkMicrolotClient r c = MkMicrolotClient . post . namespace "/apis/microlot/v1/graphql" . withoutTrailingSlash $ initRequest r c


-- * Constructors
-- $constructors


-- | Attempts to build a 'MicrolotClient' with the given DECAF Instance URL and
-- credentials.
--
-- >>> import Decaf.Client.Internal.Types
-- >>> mkMicrolotClientM "https://example.com" (CredentialsHeader "OUCH") :: Either DecafClientError MicrolotClient
-- Right (MkMicrolotClient {unMicrolotClient = Request {
--   requestRemote            = [https]://[example.com]:[443]
--   requestNamespace         = MkPath {unPath = ["apis","microlot","v1","graphql"]}
--   requestCredentials       = <********>
--   requestUserAgent         = "DECAF API Client/... (Haskell)"
--   requestHttpHeaders       = [("X-DECAF-URL","https://example.com:443")]
--   requestHttpMethod        = POST
--   requestHttpPath          = MkPath {unPath = []}
--   requestHttpTrailingSlash = False
--   requestHttpParams        = []
--   requestHttpPayload       = Nothing
-- }})
mkMicrolotClientM :: MonadError DecafClientError m => T.Text -> Credentials -> m MicrolotClient
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


-- | Runs the 'MicrolotClient' along with given 'Request' combinators and
-- returns a 'Response' value JSON-decoded from the response body.
runMicrolot :: (MonadIO m, ToJSON a, Show b, FromJSON b) => MicrolotQuery a -> MicrolotClient-> m (Response (MicrolotResponse b))
runMicrolot query cli = runRequest $ mkRequest (jsonPayload query) cli


-- * Internal
-- $internal


-- | Builds an 'Request' from a 'MicrolotClient' while applying a 'Combinator'.
mkRequest :: Combinator -> MicrolotClient -> Request
mkRequest c = c . unMicrolotClient
