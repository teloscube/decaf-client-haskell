-- | This module provides data definitions and auxiliaries to build and work
-- with 'Request' values.

module Decaf.Client.Internal.Request where

import           Control.Monad.Except              (MonadError)
import qualified Data.Aeson                        as Aeson
import qualified Data.ByteString.Lazy              as BL
import qualified Data.Text                         as T
import           Decaf.Client.Internal.Credentials
import           Decaf.Client.Internal.Error       (DecafClientError)
import           Decaf.Client.Internal.Remote      (Remote(..), parseRemote, remoteUrl)
import           Decaf.Client.Internal.Utils       (dropTrailing)
import           Decaf.Client.Version              (version)
import           Text.Printf                       (printf)


-- * Data Definition
-- $dataDefinition


-- | Type definition for high-level encoding of DECAF client requests.
data Request = Request
  { requestRemote            :: !Remote
  , requestNamespace         :: !Path
  , requestCredentials       :: !Credentials
  , requestUserAgent         :: !T.Text
  , requestHttpHeaders       :: !Headers
  , requestHttpMethod        :: !Method
  , requestHttpPath          :: !Path
  , requestHttpTrailingSlash :: !Bool
  , requestHttpParams        :: ![Param]
  , requestHttpPayload       :: !(Maybe Payload)
  }


instance Show Request where
  show x = dropTrailing '\n' $ unlines
    [ "Request {"
    , "  requestRemote            = " <> show (requestRemote x)
    , "  requestNamespace         = " <> show (requestNamespace x)
    , "  requestCredentials       = " <> show (requestCredentials x)
    , "  requestUserAgent         = " <> show (requestUserAgent x)
    , "  requestHttpHeaders       = " <> show (requestHttpHeaders x)
    , "  requestHttpMethod        = " <> show (requestHttpMethod x)
    , "  requestHttpPath          = " <> show (requestHttpPath x)
    , "  requestHttpTrailingSlash = " <> show (requestHttpTrailingSlash x)
    , "  requestHttpParams        = " <> show (requestHttpParams x)
    , "  requestHttpPayload       = " <> show (requestHttpPayload x)
    , "}"
    ]


-- | Available DECAF endpoint methods.
data Method = GET | POST | PUT | DELETE deriving (Show)


-- | Type definition for DECAF client request header key/value pair.
type Header = (T.Text, T.Text)


-- | Type definition for a list of DECAF client request 'Header's.
type Headers = [Header]


-- | Data definition for DECAF client request payloads.
data Payload = Payload
  { payloadType    :: !T.Text         -- ^ HTTP content type.
  , payloadContent :: !BL.ByteString  -- ^ HTTP payload body.
  }


instance Show Payload where
  show _ = "<TRUNCATED>"


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


-- * Request Parameters
-- $requestParameters


-- | Type definition for DECAF client request querystring parameter.
type Param = (T.Text, T.Text)


-- | Type definition for a list of DECAF client request 'Param's.
type Params = [Param]


-- * Request Paths
-- $requestPaths


-- | Type definition for a list of DECAF client request HTTP path segments.
newtype Path = MkPath { unPath :: [T.Text] } deriving Show


-- >>> mkPath "/a/b" <> mkPath "/c/d"
-- MkPath {unPath = ["a","b","c","d"]}
instance Semigroup Path where
  (<>) (MkPath p1) (MkPath p2) = MkPath (p1 <> p2)


-- >>> mconcat [mkPath "/a/b", mkPath "/c/d"]
-- MkPath {unPath = ["a","b","c","d"]}
instance Monoid Path where
  mempty = MkPath []
  mappend = (<>)
  mconcat ps = MkPath $ concatMap unPath ps


-- | Sanitizes the given 'T.Text' into path segments and builds a 'Path' value.
--
-- >>> mkPath ""
-- MkPath {unPath = []}
-- >>> mkPath "/"
-- MkPath {unPath = []}
-- >>> mkPath "//"
-- MkPath {unPath = []}
-- >>> mkPath "/a"
-- MkPath {unPath = ["a"]}
-- >>> mkPath "a/"
-- MkPath {unPath = ["a"]}
-- >>> mkPath "/a/"
-- MkPath {unPath = ["a"]}
-- >>> mkPath "/a/b"
-- MkPath {unPath = ["a","b"]}
-- >>> mkPath "a/b/"
-- MkPath {unPath = ["a","b"]}
-- >>> mkPath "/a/b/"
-- MkPath {unPath = ["a","b"]}
mkPath :: T.Text -> Path
mkPath = MkPath . filter ("" /=) . T.split ('/' ==)


-- * Combinators
-- $combinators


-- | Type definition of 'Request' combinator.
type Combinator = Request -> Request


-- | Sets the DECAF Instance 'Remote' address.
setRemote :: Remote -> Combinator
setRemote h request = request { requestRemote = h }


-- | Alias to 'setRemote'.
remote :: Remote -> Combinator
remote = setRemote


-- | Sets the namespace of the particular DECAF API.
--
-- >>> import Decaf.Client.Internal.Request (defaultRequest)
-- >>> setNamespace (mkPath "api") defaultRequest
-- Request {
--   requestRemote            = [http]://[localhost]:[80]
--   requestNamespace         = MkPath {unPath = ["api"]}
--   requestCredentials       = <********>
--   requestUserAgent         = "DECAF API Client/... (Haskell)"
--   requestHttpHeaders       = []
--   requestHttpMethod        = GET
--   requestHttpPath          = MkPath {unPath = []}
--   requestHttpTrailingSlash = False
--   requestHttpParams        = []
--   requestHttpPayload       = Nothing
-- }
setNamespace :: Path -> Combinator
setNamespace n request = request { requestNamespace = n }


-- | Sets the namespace of the particular DECAF API from a given 'T.Text' value.
--
-- >>> import Decaf.Client.Internal.Request (defaultRequest)
-- >>> namespace "///api///" defaultRequest
-- Request {
--   requestRemote            = [http]://[localhost]:[80]
--   requestNamespace         = MkPath {unPath = ["api"]}
--   requestCredentials       = <********>
--   requestUserAgent         = "DECAF API Client/... (Haskell)"
--   requestHttpHeaders       = []
--   requestHttpMethod        = GET
--   requestHttpPath          = MkPath {unPath = []}
--   requestHttpTrailingSlash = False
--   requestHttpParams        = []
--   requestHttpPayload       = Nothing
-- }
namespace :: T.Text -> Combinator
namespace = setNamespace . mkPath


-- | Sets the authentication credentials.
setCredentials :: Credentials -> Combinator
setCredentials c request = request { requestCredentials = c }


-- | Alias to 'setCredentials'.
credentials :: Credentials -> Combinator
credentials = setCredentials


-- | Sets the user-agent.
setUserAgent :: T.Text -> Combinator
setUserAgent ua request = request { requestUserAgent = ua}


-- | Alias to 'setUserAgent'.
userAgent :: T.Text -> Combinator
userAgent = setUserAgent


-- | Sets 'Request' 'Headers'.
--
-- This combinator removes all existing user-set headers.
setHeaders :: Headers -> Combinator
setHeaders hs request = request { requestHttpHeaders = hs }


-- | Adds more 'Request' 'Headers'.
--
-- This combinator does NOT remove existing user-set headers, but overwrites if
-- keys match.
addHeaders :: Headers -> Combinator
addHeaders hs request = setHeaders (existing <> hs) request
  where
    headkeys = fmap fst hs
    existing = filter (\x -> fst x `notElem` headkeys) . requestHttpHeaders $ request


-- | Alias to 'setHeaders'.
headers :: Headers -> Combinator
headers = setHeaders


-- | Adds a new 'Header' or overwrites an existing 'Header' if it exists.
addHeader :: Header -> Combinator
addHeader h = addHeaders [h]


-- | Convenient alternative to 'addHeader'.
header :: T.Text -> T.Text -> Combinator
header k v = addHeader (k, v)


-- | Sets the 'Request' method.
setMethod :: Method -> Combinator
setMethod m request = request { requestHttpMethod = m }


-- | Makes the 'Request' a @GET@ 'Request'.
get :: Combinator
get = setMethod GET


-- | Makes the 'Request' a @POST@ 'Request'.
post :: Combinator
post = setMethod POST


-- | Makes the 'Request' a @PUT@ 'Request'.
put :: Combinator
put = setMethod PUT


-- | Makes the 'Request' a @DELETE@ 'Request'.
delete :: Combinator
delete = setMethod DELETE


-- | Sets the 'Request' 'Path'.
setPath :: Path -> Combinator
setPath p request = request { requestHttpPath = p}


-- | Appends a 'Path' to the 'Request'\'s existing 'Path'.
addPath :: Path -> Combinator
addPath p request = setPath (requestHttpPath request <> p) request


-- | Convenience function to append a plain 'T.Text' as a path to the existing
-- 'Path' of the 'Request'.
path :: T.Text -> Combinator
path = addPath . mkPath


-- | Makes the 'Request' 'Path' (not) contain a trailing slash when hitting the
-- remote.
setTrailingSlash :: Bool -> Combinator
setTrailingSlash ts request = request { requestHttpTrailingSlash = ts }


-- | Makes the 'Request' 'Path' contain a trailing slash when hitting the
-- remote.
withTrailingSlash :: Combinator
withTrailingSlash = setTrailingSlash True


-- | Makes the 'Request' 'Path' not contain a trailing slash when hitting the
-- remote.
withoutTrailingSlash :: Combinator
withoutTrailingSlash = setTrailingSlash False


-- | Sets the 'Request' 'Params'.
setParams :: Params -> Combinator
setParams ps request = request { requestHttpParams = ps}


-- | Appends more 'Params' to the 'Request'\'s 'Params'.
addParams :: Params -> Combinator
addParams ps request = setParams (requestHttpParams request <> ps) request


-- | Alias to 'addParams'.
params :: Params -> Combinator
params = addParams


-- | Appends a 'Param' to the 'Request'\'s 'Params'.
addParam :: Param -> Combinator
addParam p = addParams [p]


-- | Alias to 'addParam'
param :: Param -> Combinator
param = addParam


-- | Sets the 'Request' 'Payload'.
setPayload :: T.Text -> BL.ByteString -> Combinator
setPayload t c request = request { requestHttpPayload = Just $ Payload t c }


-- | Alias to 'setPayload'.
payload :: T.Text -> BL.ByteString -> Combinator
payload = setPayload


-- | Sets a JSON 'Payload' as the 'Request' 'Payload'.
jsonPayload :: Aeson.ToJSON a => a -> Combinator
jsonPayload x = setPayload "application/json" $ Aeson.encode x


-- | Removes any existing 'Request' 'Payload'.
setNoPayload :: Combinator
setNoPayload request = request { requestHttpPayload = Nothing }


-- | Alias to 'setNoPayload'.
noPayload :: Combinator
noPayload = setNoPayload


-- | /Dummy definition./
--
-- __TODO:__ See and follow https://github.com/haskell/haddock/issues/958
_dummyDef :: ()
_dummyDef = undefined
