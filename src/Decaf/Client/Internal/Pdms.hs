-- | This module provides a DECAF PDMS module client implementation.
--
module Decaf.Client.Internal.Pdms where

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
import qualified Decaf.Client.Internal.Remote      as IRemote
import qualified Decaf.Client.Internal.Request     as IR
import qualified Decaf.Client.Internal.Types       as IT
import           Decaf.Client.Internal.Utils       (applyFirst)
import           GHC.Generics                      (Generic)


-- | DECAF PDMS API client type.
--
-- This is a _wrapper_ around 'IT.Request'.
newtype PdmsClient = MkPdmsClient { unPdmsClient :: IT.Request } deriving Show


-- | Builds a 'PdmsClient' with the given DECAF deployment 'IRemote.Remote' and credentials.
--
-- >>> mkPdmsClient (IT.Remote "example.com" Nothing True) (IT.HeaderCredentials "OUCH") :: PdmsClient
-- MkPdmsClient {unPdmsClient = Request {
--   requestRemote            = [https]://[example.com]:[443]
--   requestNamespace         = MkPath {unPath = ["apis","modules","pdms","v1","graphql"]}
--   requestCredentials       = <********>
--   requestUserAgent         = "DECAF API Client/0.0.0.2 (Haskell)"
--   requestHttpHeaders       = [("X-DECAF-URL","https://example.com:443")]
--   requestHttpMethod        = POST
--   requestHttpPath          = MkPath {unPath = []}
--   requestHttpTrailingSlash = False
--   requestHttpParams        = []
--   requestHttpPayload       = Nothing
-- }}
mkPdmsClient :: IT.Remote -> IT.Credentials -> PdmsClient
mkPdmsClient r c = MkPdmsClient . IC.post . IC.namespace "/apis/modules/pdms/v1/graphql" . IC.withoutTrailingSlash $ IR.initRequest r c


-- | Attempts to build a 'PdmsClient' with the given DECAF deployment URL and credentials.
--
-- >>> mkPdmsClientM "https://example.com" (IT.HeaderCredentials "OUCH") :: Either IT.DecafClientError PdmsClient
-- Right (MkPdmsClient {unPdmsClient = Request {
--   requestRemote            = [https]://[example.com]:[443]
--   requestNamespace         = MkPath {unPath = ["apis","modules","pdms","v1","graphql"]}
--   requestCredentials       = <********>
--   requestUserAgent         = "DECAF API Client/0.0.0.2 (Haskell)"
--   requestHttpHeaders       = [("X-DECAF-URL","https://example.com:443")]
--   requestHttpMethod        = POST
--   requestHttpPath          = MkPath {unPath = []}
--   requestHttpTrailingSlash = False
--   requestHttpParams        = []
--   requestHttpPayload       = Nothing
-- }})
mkPdmsClientM :: IT.DecafClientM m => T.Text -> IT.Credentials -> m PdmsClient
mkPdmsClientM d c = (`mkPdmsClient` c) <$> IRemote.parseRemote d


-- | Runs the 'PdmsClient' along with given 'IR.Request' combinators and
-- returns a 'IV.Response' value JSON-decoded from the response body.
runPdms :: (MonadIO m, ToJSON a, Show b, FromJSON b) => PdmsQuery a -> PdmsClient-> m (IT.Response (PdmsResponse b))
runPdms query cli = IH.runRequest $ mkRequest (IC.jsonPayload query) cli


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


-- | PDMS response definition.
data PdmsResponse a = PdmsResponse
  { pdmsResponseData   :: !a
  , pdmsResponseErrors :: !(Maybe (NonEmpty Object))
  } deriving (Generic, Show)

instance (FromJSON a) => FromJSON (PdmsResponse a) where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = applyFirst toLower . drop 12 }


--------------------
-- BEGIN INTERNAL --
--------------------


-- | Builds an 'IT.Request' from a 'PdmsClient' while applying a 'IC.Combinator'.
mkRequest :: IC.Combinator -> PdmsClient -> IT.Request
mkRequest c = c . unPdmsClient


------------------
-- END INTERNAL --
------------------
