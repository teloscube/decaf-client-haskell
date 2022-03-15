-- | This module provides auxiliaries to build and work with 'Request' values.

module Decaf.Client.Internal.Request where

import           Control.Monad.Except              (MonadError)
import qualified Data.Text                         as T
import           Decaf.Client.Internal.Combinators (credentials, header, remote)
import           Decaf.Client.Internal.Error       (DecafClientError)
import           Decaf.Client.Internal.Remote      (Remote(..), parseRemote, remoteUrl)
import           Decaf.Client.Internal.Types       (Credentials(..), Method(..), Request(..))
import           Decaf.Client.Version              (version)
import           Text.Printf                       (printf)


-- * Request Initializers
-- $requestInitializers


-- | Initializes a request with DECAF Instance URL and authentication credentials.
--
-- >>> initRequest (Remote "example.com" Nothing False) (CredentialsHeader "OUCH")
-- Request {
--   requestRemote            = [http]://[example.com]:[80]
--   requestNamespace         = MkPath {unPath = []}
--   requestCredentials       = <********>
--   requestUserAgent         = "DECAF API Client/... (Haskell)"
--   requestHttpHeaders       = [("X-DECAF-URL","http://example.com:80")]
--   requestHttpMethod        = GET
--   requestHttpPath          = MkPath {unPath = []}
--   requestHttpTrailingSlash = False
--   requestHttpParams        = []
--   requestHttpPayload       = Nothing
-- }
initRequest :: Remote -> Credentials -> Request
initRequest r c = (remote r . credentials c . header "X-DECAF-URL" (remoteUrl r)) defaultRequest


-- | Initializes a request with DECAF Instance URL and authentication credentials.
--
-- >>> import Decaf.Client
-- >>> initRequestM "http://example.com" (CredentialsHeader "OUCH") :: Either DecafClientError Request
-- Right Request {
--   requestRemote            = [http]://[example.com]:[80]
--   requestNamespace         = MkPath {unPath = []}
--   requestCredentials       = <********>
--   requestUserAgent         = "DECAF API Client/... (Haskell)"
--   requestHttpHeaders       = [("X-DECAF-URL","http://example.com:80")]
--   requestHttpMethod        = GET
--   requestHttpPath          = MkPath {unPath = []}
--   requestHttpTrailingSlash = False
--   requestHttpParams        = []
--   requestHttpPayload       = Nothing
-- }
initRequestM :: MonadError DecafClientError m => T.Text -> Credentials -> m Request
initRequestM url creds = (`initRequest` creds) <$> parseRemote url


-- * Internal
-- $internal


-- | Default 'Request'.
--
-- This is useful to build 'Request' values using combinators.
defaultRequest :: Request
defaultRequest = Request
  { requestRemote = Remote "localhost" Nothing False
  , requestNamespace = mempty
  , requestCredentials = CredentialsHeader "UNKNOWN"
  , requestUserAgent = defaultUserAgent
  , requestHttpHeaders = []
  , requestHttpMethod = GET
  , requestHttpPath = mempty
  , requestHttpTrailingSlash = False
  , requestHttpParams = []
  , requestHttpPayload = Nothing
  }


-- | User agent value definition for the library.
--
-- >>> defaultUserAgent
-- "DECAF API Client/... (Haskell)"
defaultUserAgent :: T.Text
defaultUserAgent = T.pack $ printf "DECAF API Client/%s (Haskell)" version
