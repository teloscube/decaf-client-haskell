-- | This module provides a DECAF Barista API client implementation.

module Decaf.Client.Internal.Apis.Barista where

import           Control.Monad.Except              (MonadError)
import           Control.Monad.IO.Class            (MonadIO)
import           Data.Aeson                        (FromJSON)
import qualified Data.ByteString                   as B
import qualified Data.Text                         as T
import           Decaf.Client.DecafRemote          (DecafRemote, parseRemote)
import           Decaf.Client.DecafRequest
                 ( DecafRequest
                 , DecafRequestCombinator
                 , initRequest
                 , namespace
                 , withTrailingSlash
                 )
import           Decaf.Client.DecafResponse        (DecafResponse)
import           Decaf.Client.Internal.Credentials (Credentials)
import           Decaf.Client.Internal.Error       (DecafClientError)
import           Decaf.Client.Internal.Http        (runRequest, runRequestBS)


-- * Data Definition
-- $dataDefinition


-- | DECAF Barista API client type.
newtype BaristaClient = MkBaristaClient { unBaristaClient :: DecafRequest } deriving Show


-- * Constructors
-- $constructors


-- | Builds a 'BaristaClient' with the given DECAF Instance 'Remote' and
-- credentials.
--
-- >>> import Decaf.Client.Internal.Credentials
-- >>> import Decaf.Client.DecafRemote
-- >>> mkBaristaClient (Remote "example.com" Nothing True) (CredentialsHeader "OUCH") :: BaristaClient
-- MkBaristaClient {unBaristaClient = Request {
--   requestRemote            = [https]://[example.com]:[443]
--   requestNamespace         = MkDecafRequestPath {unDecafRequestPath = ["api"]}
--   requestCredentials       = <********>
--   requestUserAgent         = "DECAF API Client/... (Haskell)"
--   requestHttpHeaders       = [("X-DECAF-URL","https://example.com:443")]
--   requestHttpMethod        = GET
--   requestHttpPath          = MkDecafRequestPath {unDecafRequestPath = []}
--   requestHttpTrailingSlash = True
--   requestHttpQuery         = []
--   requestHttpPayload       = Nothing
-- }}
mkBaristaClient :: DecafRemote -> Credentials -> BaristaClient
mkBaristaClient r c = MkBaristaClient . namespace "api" . withTrailingSlash $ initRequest r c


-- | Attempts to build a 'BaristaClient' with the given DECAF Instance URL and
-- credentials.
--
-- >>> import Decaf.Client.Internal.Credentials
-- >>> mkBaristaClientM "https://example.com" (CredentialsHeader "OUCH") :: Either DecafClientError BaristaClient
-- Right (MkBaristaClient {unBaristaClient = Request {
--   requestRemote            = [https]://[example.com]:[443]
--   requestNamespace         = MkDecafRequestPath {unDecafRequestPath = ["api"]}
--   requestCredentials       = <********>
--   requestUserAgent         = "DECAF API Client/... (Haskell)"
--   requestHttpHeaders       = [("X-DECAF-URL","https://example.com:443")]
--   requestHttpMethod        = GET
--   requestHttpPath          = MkDecafRequestPath {unDecafRequestPath = []}
--   requestHttpTrailingSlash = True
--   requestHttpQuery         = []
--   requestHttpPayload       = Nothing
-- }})
mkBaristaClientM :: MonadError DecafClientError m => T.Text -> Credentials -> m BaristaClient
mkBaristaClientM d c = (`mkBaristaClient` c) <$> parseRemote d


-- * Runners
-- $runners


-- | Runs the 'BaristaClient' along with given 'DecafRequest' combinators and returns
-- a 'Response' value JSON-decoded from the response body.
runBarista :: (MonadIO m, FromJSON a) => DecafRequestCombinator -> BaristaClient -> m (DecafResponse a)
runBarista cmb cli = runRequest $ mkRequest cmb cli


-- | Runs the 'BaristaClient' along with given 'DecafRequest' combinators and returns
-- a 'Response' with 'B.ByteString' value.
runBaristaBS :: MonadIO m => DecafRequestCombinator -> BaristaClient -> m (DecafResponse B.ByteString)
runBaristaBS cmb cli = runRequestBS $ mkRequest cmb cli


-- * Internal
-- $internal


-- | Builds an 'DecafRequest' from a 'BaristaClient' while applying a 'Combinator'.
mkRequest :: DecafRequestCombinator -> BaristaClient -> DecafRequest
mkRequest c = c . unBaristaClient
