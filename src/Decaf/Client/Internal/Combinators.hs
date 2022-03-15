-- | This module provides combinators to modify 'Request' values.
--
module Decaf.Client.Internal.Combinators where

import           Data.Aeson                  (ToJSON, encode)
import qualified Data.ByteString.Lazy        as BL
import qualified Data.Text                   as T
import           Decaf.Client.Internal.Types
                 ( Credentials
                 , Header
                 , Headers
                 , Method(..)
                 , Param
                 , Params
                 , Path
                 , Payload(..)
                 , Remote
                 , Request(..)
                 , mkPath
                 )


-- | Type definition of 'I.Request' combinator.
type Combinator = Request -> Request


-- | Sets the DECAF deployment 'Remote' address.
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
--   requestUserAgent         = "DECAF API Client/0.0.0.2 (Haskell)"
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
--   requestUserAgent         = "DECAF API Client/0.0.0.2 (Haskell)"
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
-- This combinator does NOT remove existing user-set headers, but overwrites if keys match.
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


-- | Convenience function to append a plain 'T.Text' as a path to the existing 'Path' of the 'Request'.
path :: T.Text -> Combinator
path = addPath . mkPath


-- | Makes the 'Request' 'Path' (not) contain a trailing slash when hitting the remote.
setTrailingSlash :: Bool -> Combinator
setTrailingSlash ts request = request { requestHttpTrailingSlash = ts }


-- | Makes the 'Request' 'Path' contain a trailing slash when hitting the remote.
withTrailingSlash :: Combinator
withTrailingSlash = setTrailingSlash True


-- | Makes the 'Request' 'Path' not contain a trailing slash when hitting the remote.
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
jsonPayload :: ToJSON a => a -> Combinator
jsonPayload x = setPayload "application/json" $ encode x


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
