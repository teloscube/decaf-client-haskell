-- | This module provides a DECAF PDMS module client implementation.
--
module Decaf.Client.Internal.Apis.Pdms where

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
import           Decaf.Client.DecafRemote          (DecafRemote, parseRemote)
import           Decaf.Client.DecafRequest
                 ( DecafRequest
                 , DecafRequestCombinator
                 , initRequest
                 , jsonPayload
                 , namespace
                 , post
                 , withoutTrailingSlash
                 )
import           Decaf.Client.DecafResponse        (DecafResponse)
import           Decaf.Client.Internal.Credentials (Credentials)
import           Decaf.Client.Internal.Error       (DecafClientError)
import           Decaf.Client.Internal.Http        (runRequest)
import           Decaf.Client.Internal.Utils       (applyFirst)
import           GHC.Generics                      (Generic)


-- * Data Definition
-- $dataDefinition


-- | DECAF PDMS module API client type.
--
-- This is a /wrapper/ around 'DecafRequest'.
newtype PdmsClient = MkPdmsClient { unPdmsClient :: DecafRequest } deriving Show


-- * Constructors
-- $constructors


-- | Builds a 'PdmsClient' with the given DECAF Instance 'Remote' and
-- credentials.
--
-- >>> import Decaf.Client.Internal.Credentials
-- >>> import Decaf.Client.DecafRemote
-- >>> mkPdmsClient (Remote "example.com" Nothing True) (CredentialsHeader "OUCH") :: PdmsClient
-- MkPdmsClient {unPdmsClient = Request {
--   requestRemote            = [https]://[example.com]:[443]
--   requestNamespace         = MkDecafRequestPath {unDecafRequestPath = ["apis","modules","pdms","v1","graphql"]}
--   requestCredentials       = <********>
--   requestUserAgent         = "DECAF API Client/... (Haskell)"
--   requestHttpHeaders       = [("X-DECAF-URL","https://example.com:443")]
--   requestHttpMethod        = POST
--   requestHttpPath          = MkDecafRequestPath {unDecafRequestPath = []}
--   requestHttpTrailingSlash = False
--   requestHttpQuery         = []
--   requestHttpPayload       = Nothing
-- }}
mkPdmsClient :: DecafRemote -> Credentials -> PdmsClient
mkPdmsClient r c = MkPdmsClient . post . namespace "/apis/modules/pdms/v1/graphql" . withoutTrailingSlash $ initRequest r c


-- | Attempts to build a 'PdmsClient' with the given DECAF Instance URL and credentials.
--
-- >>> import Decaf.Client.Internal.Credentials
-- >>> import Decaf.Client.DecafRemote
-- >>> mkPdmsClientM "https://example.com" (CredentialsHeader "OUCH") :: Either DecafClientError PdmsClient
-- Right (MkPdmsClient {unPdmsClient = Request {
--   requestRemote            = [https]://[example.com]:[443]
--   requestNamespace         = MkDecafRequestPath {unDecafRequestPath = ["apis","modules","pdms","v1","graphql"]}
--   requestCredentials       = <********>
--   requestUserAgent         = "DECAF API Client/... (Haskell)"
--   requestHttpHeaders       = [("X-DECAF-URL","https://example.com:443")]
--   requestHttpMethod        = POST
--   requestHttpPath          = MkDecafRequestPath {unDecafRequestPath = []}
--   requestHttpTrailingSlash = False
--   requestHttpQuery         = []
--   requestHttpPayload       = Nothing
-- }})
mkPdmsClientM :: MonadError DecafClientError m => T.Text -> Credentials -> m PdmsClient
mkPdmsClientM d c = (`mkPdmsClient` c) <$> parseRemote d


-- * Queries
-- $queries


-- | PDMS query type as a sealed query/variables tuple.
data PdmsQuery a = ToJSON a => MkPdmsQuery !String !a


instance ToJSON (PdmsQuery a) where
  toJSON (MkPdmsQuery q v) = object ["query" .= q, "variables" .= v]


-- | Builds a 'PdmsQuery' with the given GraphQL query and GraphQL query variables.
mkPdmsQuery :: ToJSON a => String -> a -> PdmsQuery a
mkPdmsQuery = MkPdmsQuery


-- | Builds a 'PdmsQuery' with the given GraphQL query (without GraphQL query variables).
mkPdmsQuery' :: String -> PdmsQuery Value
mkPdmsQuery' = flip MkPdmsQuery $ object []


-- * Response
-- $response


-- | PDMS response definition.
data PdmsResponse a = PdmsResponse
  { pdmsResponseData   :: !a
  , pdmsResponseErrors :: !(Maybe (NonEmpty Object))
  } deriving (Generic, Show)


instance (FromJSON a) => FromJSON (PdmsResponse a) where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = applyFirst toLower . drop 12 }


-- * Runners
-- $runners


-- | Runs the 'PdmsClient' along with given 'IR.Request' combinators and
-- returns a 'IV.Response' value JSON-decoded from the response body.
runPdms :: (MonadIO m, ToJSON a, Show b, FromJSON b) => PdmsQuery a -> PdmsClient-> m (DecafResponse (PdmsResponse b))
runPdms query cli = runRequest $ mkRequest (jsonPayload query) cli


-- * Internal
-- $internal


-- | Builds an 'DecafRequest' from a 'PdmsClient' while applying a 'Combinator'.
mkRequest :: DecafRequestCombinator -> PdmsClient -> DecafRequest
mkRequest c = c . unPdmsClient
