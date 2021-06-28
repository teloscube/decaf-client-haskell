-- | This module provides auxiliaries to build and work with 'Request' values.
--
module Decaf.Client.Internal.Request where

import qualified Data.Text                         as T
import           Decaf.Client.Internal.Combinators (credentials, header, remote)
import           Decaf.Client.Internal.Remote      (parseRemote, remoteUrl)
import           Decaf.Client.Internal.Types
                 ( Credentials(HeaderCredentials)
                 , DecafClientM
                 , Method(GET)
                 , Remote(Remote)
                 , Request(..)
                 )
import           Decaf.Client.Version              (version)
import           Text.Printf                       (printf)


-- | Initializes a request with deployment URL and authentication credentials.
--
-- >>> initRequest (Remote "example.com" Nothing False) (HeaderCredentials "OUCH")
-- Request {
--   requestRemote            = [http]://[example.com]:[80]
--   requestNamespace         = MkPath {unPath = []}
--   requestCredentials       = <********>
--   requestUserAgent         = "DECAF API Client/0.0.0.1 (Haskell)"
--   requestHttpHeaders       = [("X-DECAF-URL","http://example.com:80")]
--   requestHttpMethod        = GET
--   requestHttpPath          = MkPath {unPath = []}
--   requestHttpTrailingSlash = False
--   requestHttpParams        = []
--   requestHttpPayload       = Nothing
-- }
initRequest :: Remote -> Credentials -> Request
initRequest r c = (remote r . credentials c . header "X-DECAF-URL" (remoteUrl r)) defaultRequest


-- | Initializes a request with deployment URL and authentication credentials.
--
-- >>> initRequestM "http://example.com" (HeaderCredentials "OUCH") :: Either DecafClientError Request
-- Right Request {
--   requestRemote            = [http]://[example.com]:[80]
--   requestNamespace         = MkPath {unPath = []}
--   requestCredentials       = <********>
--   requestUserAgent         = "DECAF API Client/0.0.0.1 (Haskell)"
--   requestHttpHeaders       = [("X-DECAF-URL","http://example.com:80")]
--   requestHttpMethod        = GET
--   requestHttpPath          = MkPath {unPath = []}
--   requestHttpTrailingSlash = False
--   requestHttpParams        = []
--   requestHttpPayload       = Nothing
-- }
initRequestM :: DecafClientM m => T.Text -> Credentials -> m Request
initRequestM deployment c = (`initRequest` c) <$> parseRemote deployment


--------------------
-- BEGIN INTERNAL --
--------------------

-- | Default 'Request'.
--
-- This is useful to build 'Request' values using combinators.
defaultRequest :: Request
defaultRequest = Request
  { requestRemote = Remote "localhost" Nothing False
  , requestNamespace = mempty
  , requestCredentials = HeaderCredentials "UNKNOWN"
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
-- "DECAF API Client/0.0.0.1 (Haskell)"
defaultUserAgent :: T.Text
defaultUserAgent = T.pack $ printf "DECAF API Client/%s (Haskell)" version
