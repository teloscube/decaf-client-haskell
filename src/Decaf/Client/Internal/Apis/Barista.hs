-- | This module provides a DECAF Barista API client implementation.

module Decaf.Client.Internal.Apis.Barista where

import           Control.Monad.Except              (MonadError)
import           Control.Monad.IO.Class            (MonadIO)
import           Data.Aeson                        (FromJSON)
import qualified Data.ByteString                   as B
import qualified Data.Text                         as T
import           Decaf.Client.Internal.Combinators (Combinator, namespace, withTrailingSlash)
import           Decaf.Client.Internal.Error       (DecafClientError)
import           Decaf.Client.Internal.Http        (runRequest, runRequestBS)
import           Decaf.Client.Internal.Remote      (parseRemote)
import           Decaf.Client.Internal.Request     (initRequest)
import           Decaf.Client.Internal.Types       (Credentials, Remote, Request, Response)


-- * Data Definition
-- $dataDefinition


-- | DECAF Barista API client type.
newtype BaristaClient = MkBaristaClient { unBaristaClient :: Request } deriving Show


-- * Constructors
-- $constructors


-- | Builds a 'BaristaClient' with the given DECAF Instance 'Remote' and
-- credentials.
--
-- >>> import Decaf.Client.Internal.Types
-- >>> mkBaristaClient (Remote "example.com" Nothing True) (CredentialsHeader "OUCH") :: BaristaClient
-- MkBaristaClient {unBaristaClient = Request {
--   requestRemote            = [https]://[example.com]:[443]
--   requestNamespace         = MkPath {unPath = ["api"]}
--   requestCredentials       = <********>
--   requestUserAgent         = "DECAF API Client/... (Haskell)"
--   requestHttpHeaders       = [("X-DECAF-URL","https://example.com:443")]
--   requestHttpMethod        = GET
--   requestHttpPath          = MkPath {unPath = []}
--   requestHttpTrailingSlash = True
--   requestHttpParams        = []
--   requestHttpPayload       = Nothing
-- }}
mkBaristaClient :: Remote -> Credentials -> BaristaClient
mkBaristaClient r c = MkBaristaClient . namespace "api" . withTrailingSlash $ initRequest r c


-- | Attempts to build a 'BaristaClient' with the given DECAF Instance URL and
-- credentials.
--
-- >>> import Decaf.Client.Internal.Types
-- >>> mkBaristaClientM "https://example.com" (CredentialsHeader "OUCH") :: Either DecafClientError BaristaClient
-- Right (MkBaristaClient {unBaristaClient = Request {
--   requestRemote            = [https]://[example.com]:[443]
--   requestNamespace         = MkPath {unPath = ["api"]}
--   requestCredentials       = <********>
--   requestUserAgent         = "DECAF API Client/... (Haskell)"
--   requestHttpHeaders       = [("X-DECAF-URL","https://example.com:443")]
--   requestHttpMethod        = GET
--   requestHttpPath          = MkPath {unPath = []}
--   requestHttpTrailingSlash = True
--   requestHttpParams        = []
--   requestHttpPayload       = Nothing
-- }})
mkBaristaClientM :: MonadError DecafClientError m => T.Text -> Credentials -> m BaristaClient
mkBaristaClientM d c = (`mkBaristaClient` c) <$> parseRemote d


-- * Runners
-- $runners


-- | Runs the 'BaristaClient' along with given 'Request' combinators and returns
-- a 'Response' value JSON-decoded from the response body.
runBarista :: (MonadIO m, FromJSON a) => Combinator -> BaristaClient -> m (Response a)
runBarista cmb cli = runRequest $ mkRequest cmb cli


-- | Runs the 'BaristaClient' along with given 'Request' combinators and returns
-- a 'Response' with 'B.ByteString' value.
runBaristaBS :: MonadIO m => Combinator -> BaristaClient -> m (Response B.ByteString)
runBaristaBS cmb cli = runRequestBS $ mkRequest cmb cli


-- * Internal
-- $internal


-- | Builds an 'Request' from a 'BaristaClient' while applying a 'Combinator'.
mkRequest :: Combinator -> BaristaClient -> Request
mkRequest c = c . unBaristaClient
