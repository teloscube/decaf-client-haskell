-- | This module provides definitions to work with DECAF client requests.

module Decaf.Client.DecafRequest where

import           Control.Monad.Except          (MonadError)
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString.Lazy          as BL
import qualified Data.CaseInsensitive          as CI
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           Decaf.Client.DecafCredentials (DecafCredentials(DecafCredentialsHeader))
import           Decaf.Client.DecafRemote      (DecafRemote(..), parseRemote, remoteToUrl)
import           Decaf.Client.Internal.Error   (DecafClientError)
import           Decaf.Client.Internal.Utils   (dropTrailing)
import           Decaf.Client.Version          (version)
import           Network.HTTP.Types
                 ( Header
                 , QueryText
                 , RequestHeaders
                 , StdMethod(DELETE, GET, PATCH, POST, PUT)
                 )
import           Text.Printf                   (printf)


-- * Data Definitions
-- $dataDefinitions


-- | Type definition for high-level encoding of DECAF client request values.
data DecafRequest = DecafRequest
  { decafRequestRemote        :: !DecafRemote
  , decafRequestNamespace     :: !DecafRequestPath
  , decafRequestCredentials   :: !DecafCredentials
  , decafRequestUserAgent     :: !T.Text
  , decafRequestHeaders       :: !RequestHeaders
  , decafRequestMethod        :: !StdMethod
  , decafRequestPath          :: !DecafRequestPath
  , decafRequestTrailingSlash :: !Bool
  , decafRequestQuery         :: !QueryText
  , decafRequestPayload       :: !(Maybe DecafRequestPayload)
  }


instance Show DecafRequest where
  show x = dropTrailing '\n' $ unlines
    [ "DecafRequest {"
    , "  decafRequestRemote        = " <> show (decafRequestRemote x)
    , "  decafRequestNamespace     = " <> show (decafRequestNamespace x)
    , "  decafRequestCredentials   = " <> show (decafRequestCredentials x)
    , "  decafRequestUserAgent     = " <> show (decafRequestUserAgent x)
    , "  decafRequestHeaders       = " <> show (decafRequestHeaders x)
    , "  decafRequestMethod        = " <> show (decafRequestMethod x)
    , "  decafRequestPath          = " <> show (decafRequestPath x)
    , "  decafRequestTrailingSlash = " <> show (decafRequestTrailingSlash x)
    , "  decafRequestQuery         = " <> show (decafRequestQuery x)
    , "  decafRequestPayload       = " <> show (decafRequestPayload x)
    , "}"
    ]


-- | Data definition for DECAF client request payloads.
data DecafRequestPayload = DecafRequestPayload
  { decafRequestPayloadType    :: !T.Text         -- ^ HTTP content type.
  , decafRequestPayloadContent :: !BL.ByteString  -- ^ HTTP payload body.
  }


instance Show DecafRequestPayload where
  show _ = "<TRUNCATED>"


-- | Type definition for a list of DECAF client request HTTP path segments.
newtype DecafRequestPath = MkDecafRequestPath
  { unDecafRequestPath :: [T.Text]
  }
  deriving Show


-- >>> mkDecafRequestPath "/a/b" <> mkDecafRequestPath "/c/d"
-- MkDecafRequestPath {unDecafRequestPath = ["a","b","c","d"]}
instance Semigroup DecafRequestPath where
  (<>) (MkDecafRequestPath p1) (MkDecafRequestPath p2) = MkDecafRequestPath (p1 <> p2)


-- >>> mconcat [mkDecafRequestPath "/a/b", mkDecafRequestPath "/c/d"]
-- MkDecafRequestPath {unDecafRequestPath = ["a","b","c","d"]}
instance Monoid DecafRequestPath where
  mempty = MkDecafRequestPath []
  mappend = (<>)
  mconcat ps = MkDecafRequestPath $ concatMap unDecafRequestPath ps


-- | Sanitizes the given 'T.Text' into path segments and builds a 'Path' value.
--
-- >>> mkDecafRequestPath ""
-- MkDecafRequestPath {unDecafRequestPath = []}
-- >>> mkDecafRequestPath "/"
-- MkDecafRequestPath {unDecafRequestPath = []}
-- >>> mkDecafRequestPath "//"
-- MkDecafRequestPath {unDecafRequestPath = []}
-- >>> mkDecafRequestPath "/a"
-- MkDecafRequestPath {unDecafRequestPath = ["a"]}
-- >>> mkDecafRequestPath "a/"
-- MkDecafRequestPath {unDecafRequestPath = ["a"]}
-- >>> mkDecafRequestPath "/a/"
-- MkDecafRequestPath {unDecafRequestPath = ["a"]}
-- >>> mkDecafRequestPath "/a/b"
-- MkDecafRequestPath {unDecafRequestPath = ["a","b"]}
-- >>> mkDecafRequestPath "a/b/"
-- MkDecafRequestPath {unDecafRequestPath = ["a","b"]}
-- >>> mkDecafRequestPath "/a/b/"
-- MkDecafRequestPath {unDecafRequestPath = ["a","b"]}
mkDecafRequestPath :: T.Text -> DecafRequestPath
mkDecafRequestPath = MkDecafRequestPath . filter ("" /=) . T.split ('/' ==)


-- * Request Initializers
-- $requestInitializers


-- | Initializes a request with DECAF Instance URL and authentication credentials.
--
-- >>> initRequest (DecafRemote "example.com" Nothing False) (DecafCredentialsHeader "OUCH")
-- DecafRequest {
--   decafRequestRemote        = [http]://[example.com]:[80]
--   decafRequestNamespace     = MkDecafRequestPath {unDecafRequestPath = []}
--   decafRequestCredentials   = <********>
--   decafRequestUserAgent     = "DECAF API Client/... (Haskell)"
--   decafRequestHeaders       = [("X-DECAF-URL","http://example.com")]
--   decafRequestMethod        = GET
--   decafRequestPath          = MkDecafRequestPath {unDecafRequestPath = []}
--   decafRequestTrailingSlash = False
--   decafRequestQuery         = []
--   decafRequestPayload       = Nothing
-- }
initRequest :: DecafRemote -> DecafCredentials -> DecafRequest
initRequest r c = (remote r . credentials c . header "X-DECAF-URL" (remoteToUrl r)) defaultRequest


-- | Initializes a request with DECAF Instance URL and authentication credentials.
--
-- >>> import Decaf.Client
-- >>> initRequestM "http://example.com" (DecafCredentialsHeader "OUCH") :: Either DecafClientError DecafRequest
-- Right DecafRequest {
--   decafRequestRemote        = [http]://[example.com]:[80]
--   decafRequestNamespace     = MkDecafRequestPath {unDecafRequestPath = []}
--   decafRequestCredentials   = <********>
--   decafRequestUserAgent     = "DECAF API Client/... (Haskell)"
--   decafRequestHeaders       = [("X-DECAF-URL","http://example.com")]
--   decafRequestMethod        = GET
--   decafRequestPath          = MkDecafRequestPath {unDecafRequestPath = []}
--   decafRequestTrailingSlash = False
--   decafRequestQuery         = []
--   decafRequestPayload       = Nothing
-- }
initRequestM :: MonadError DecafClientError m => T.Text -> DecafCredentials -> m DecafRequest
initRequestM url creds = (`initRequest` creds) <$> parseRemote url


-- | Default 'DecafRequest'.
--
-- This is useful to build 'DecafRequest' values using combinators.
defaultRequest :: DecafRequest
defaultRequest = DecafRequest
  { decafRequestRemote = DecafRemote "localhost" Nothing False
  , decafRequestNamespace = mempty
  , decafRequestCredentials = DecafCredentialsHeader "UNKNOWN"
  , decafRequestUserAgent = defaultUserAgent
  , decafRequestHeaders = []
  , decafRequestMethod = GET
  , decafRequestPath = mempty
  , decafRequestTrailingSlash = False
  , decafRequestQuery = []
  , decafRequestPayload = Nothing
  }


-- | User agent value definition for the library.
--
-- >>> defaultUserAgent
-- "DECAF API Client/... (Haskell)"
defaultUserAgent :: T.Text
defaultUserAgent = T.pack $ printf "DECAF API Client/%s (Haskell)" version


-- * Combinators
-- $combinators


-- | Type definition of 'DecafRequest' combinator.
type DecafRequestCombinator = DecafRequest -> DecafRequest


-- | Sets the DECAF Instance 'Remote' address.
setRemote :: DecafRemote -> DecafRequestCombinator
setRemote h request = request { decafRequestRemote = h }


-- | Alias to 'setRemote'.
remote :: DecafRemote -> DecafRequestCombinator
remote = setRemote


-- | Sets the namespace of the particular DECAF API.
--
-- >>> import Decaf.Client.DecafRequest (defaultRequest)
-- >>> setNamespace (mkDecafRequestPath "api") defaultRequest
-- DecafRequest {
--   decafRequestRemote        = [http]://[localhost]:[80]
--   decafRequestNamespace     = MkDecafRequestPath {unDecafRequestPath = ["api"]}
--   decafRequestCredentials   = <********>
--   decafRequestUserAgent     = "DECAF API Client/... (Haskell)"
--   decafRequestHeaders       = []
--   decafRequestMethod        = GET
--   decafRequestPath          = MkDecafRequestPath {unDecafRequestPath = []}
--   decafRequestTrailingSlash = False
--   decafRequestQuery         = []
--   decafRequestPayload       = Nothing
-- }
setNamespace :: DecafRequestPath -> DecafRequestCombinator
setNamespace n request = request { decafRequestNamespace = n }


-- | Sets the namespace of the particular DECAF API from a given 'T.Text' value.
--
-- >>> import Decaf.Client.DecafRequest (defaultRequest)
-- >>> namespace "///api///" defaultRequest
-- DecafRequest {
--   decafRequestRemote        = [http]://[localhost]:[80]
--   decafRequestNamespace     = MkDecafRequestPath {unDecafRequestPath = ["api"]}
--   decafRequestCredentials   = <********>
--   decafRequestUserAgent     = "DECAF API Client/... (Haskell)"
--   decafRequestHeaders       = []
--   decafRequestMethod        = GET
--   decafRequestPath          = MkDecafRequestPath {unDecafRequestPath = []}
--   decafRequestTrailingSlash = False
--   decafRequestQuery         = []
--   decafRequestPayload       = Nothing
-- }
namespace :: T.Text -> DecafRequestCombinator
namespace = setNamespace . mkDecafRequestPath


-- | Sets the authentication credentials.
setCredentials :: DecafCredentials -> DecafRequestCombinator
setCredentials c request = request { decafRequestCredentials = c }


-- | Alias to 'setCredentials'.
credentials :: DecafCredentials -> DecafRequestCombinator
credentials = setCredentials


-- | Sets the user-agent.
setUserAgent :: T.Text -> DecafRequestCombinator
setUserAgent ua request = request { decafRequestUserAgent = ua}


-- | Alias to 'setUserAgent'.
userAgent :: T.Text -> DecafRequestCombinator
userAgent = setUserAgent


-- | Sets 'DecafRequest' headers.
--
-- This combinator removes all existing user-set headers.
setHeaders :: RequestHeaders -> DecafRequestCombinator
setHeaders hs request = request { decafRequestHeaders = hs }


-- | Adds more 'DecafRequest' headers.
--
-- This combinator does NOT remove existing user-set headers, but overwrites if
-- keys match.
addHeaders :: RequestHeaders -> DecafRequestCombinator
addHeaders hs request = setHeaders (existing <> hs) request
  where
    headkeys = fmap fst hs
    existing = filter (\x -> fst x `notElem` headkeys) . decafRequestHeaders $ request


-- | Alias to 'setHeaders'.
headers :: RequestHeaders -> DecafRequestCombinator
headers = setHeaders


-- | Adds a new 'Header' or overwrites an existing 'Header' if it exists.
addHeader :: Header -> DecafRequestCombinator
addHeader h = addHeaders [h]


-- | Convenient alternative to 'addHeader'.
header :: T.Text -> T.Text -> DecafRequestCombinator
header k v = addHeader (CI.mk (TE.encodeUtf8 k), TE.encodeUtf8 v)


-- | Sets the 'DecafRequest' method.
setMethod :: StdMethod -> DecafRequestCombinator
setMethod m request = request { decafRequestMethod = m }


-- | Makes the 'DecafRequest' a @GET@ 'DecafRequest'.
get :: DecafRequestCombinator
get = setMethod GET


-- | Makes the 'DecafRequest' a @POST@ 'DecafRequest'.
post :: DecafRequestCombinator
post = setMethod POST


-- | Makes the 'DecafRequest' a @PUT@ 'DecafRequest'.
put :: DecafRequestCombinator
put = setMethod PUT


-- | Makes the 'DecafRequest' a @DELETE@ 'DecafRequest'.
delete :: DecafRequestCombinator
delete = setMethod DELETE


-- | Makes the 'DecafRequest' a @PATCH@ 'DecafRequest'.
patch :: DecafRequestCombinator
patch = setMethod PATCH


-- | Sets the 'DecafRequest' 'Path'.
setPath :: DecafRequestPath -> DecafRequestCombinator
setPath p request = request { decafRequestPath = p}


-- | Appends a 'Path' to the 'DecafRequest'\'s existing 'Path'.
addPath :: DecafRequestPath -> DecafRequestCombinator
addPath p request = setPath (decafRequestPath request <> p) request


-- | Convenience function to append a plain 'T.Text' as a path to the existing
-- 'Path' of the 'DecafRequest'.
path :: T.Text -> DecafRequestCombinator
path = addPath . mkDecafRequestPath


-- | Makes the 'DecafRequest' 'Path' (not) contain a trailing slash when hitting the
-- remote.
setTrailingSlash :: Bool -> DecafRequestCombinator
setTrailingSlash ts request = request { decafRequestTrailingSlash = ts }


-- | Makes the 'DecafRequest' 'Path' contain a trailing slash when hitting the
-- remote.
withTrailingSlash :: DecafRequestCombinator
withTrailingSlash = setTrailingSlash True


-- | Makes the 'DecafRequest' 'Path' not contain a trailing slash when hitting the
-- remote.
withoutTrailingSlash :: DecafRequestCombinator
withoutTrailingSlash = setTrailingSlash False


-- | Sets the 'DecafRequest' 'Query'.
setQuery :: QueryText -> DecafRequestCombinator
setQuery ps request = request { decafRequestQuery = ps}


-- | Appends more 'Query' to the 'DecafRequest'\'s 'Query'.
addQuery :: QueryText -> DecafRequestCombinator
addQuery ps request = setQuery (decafRequestQuery request <> ps) request


-- | Alias to 'addQuery'.
query :: QueryText -> DecafRequestCombinator
query = addQuery


-- | Appends a 'Param' to the 'DecafRequest'\'s 'Query'.
addParam :: T.Text -> Maybe T.Text -> DecafRequestCombinator
addParam k mv = addQuery [(k, mv)]


-- | Alias to 'addParam'
param :: T.Text -> Maybe T.Text -> DecafRequestCombinator
param = addParam


-- | Sets the 'DecafRequest' 'Payload'.
setPayload :: T.Text -> BL.ByteString -> DecafRequestCombinator
setPayload t c request = request { decafRequestPayload = Just $ DecafRequestPayload t c }


-- | Alias to 'setPayload'.
payload :: T.Text -> BL.ByteString -> DecafRequestCombinator
payload = setPayload


-- | Sets a JSON 'Payload' as the 'DecafRequest' 'Payload'.
jsonPayload :: Aeson.ToJSON a => a -> DecafRequestCombinator
jsonPayload x = setPayload "application/json" $ Aeson.encode x


-- | Removes any existing 'DecafRequest' 'Payload'.
setNoPayload :: DecafRequestCombinator
setNoPayload request = request { decafRequestPayload = Nothing }


-- | Alias to 'setNoPayload'.
noPayload :: DecafRequestCombinator
noPayload = setNoPayload


-- | /Dummy definition./
--
-- __TODO:__ See and follow https://github.com/haskell/haddock/issues/958
_dummyDef :: ()
_dummyDef = undefined
