-- | This module provides machinery to abstract over underlying HTTP client.
--
-- @TODO: Choose a proper monad to work with, handle various HTTP return codes for errors. @
--
module Decaf.Client.Internal.Http where

import           Control.Monad.IO.Class      (MonadIO)
import           Data.Aeson                  (FromJSON)
import           Data.Bifunctor              (bimap)
import qualified Data.ByteString             as B
import           Data.ByteString.Base64      (encode)
import qualified Data.ByteString.Char8       as BC
import qualified Data.CaseInsensitive        as CI
import           Data.Maybe                  (fromMaybe)
import qualified Data.Text.Encoding          as TE
import qualified Decaf.Client.Internal.Types as IT
import           Decaf.Client.Internal.Utils (compose)
import qualified Network.HTTP.Client         as H
import qualified Network.HTTP.Simple         as HS


-- | Runs a request and returns a 'IT.Response' value JSON-decoded from the response body.
runRequest :: (MonadIO m, FromJSON a) => IT.Request -> m (IT.Response a)
runRequest r = mkResponse <$> (HS.httpJSON . compileRequest) r


-- | Runs a request and returns a 'IT.Response' with 'B.ByteString' value.
runRequestBS :: MonadIO m => IT.Request -> m (IT.Response B.ByteString)
runRequestBS r = mkResponse <$> (HS.httpBS . compileRequest) r


--------------------
-- BEGIN INTERNAL --
--------------------


-- | Converts the underlying 'HS.Response' value into a DECAF client 'IT.Response' value.
mkResponse :: HS.Response a -> IT.Response a
mkResponse = IT.Response
  <$> HS.getResponseStatusCode
  <*> (fmap (bimap (TE.decodeUtf8 . CI.foldedCase) TE.decodeUtf8) . HS.getResponseHeaders)
  <*> HS.getResponseBody


-- | Type definition to modify 'H.Request' fields from a given 'IT.Request' value.
type RequestFieldSetter = IT.Request -> H.Request -> H.Request


-- | Compiles a DECAF 'IT.Request' into an HTTP 'H.Request'.
compileRequest :: IT.Request -> H.Request
compileRequest request = compiler request HS.defaultRequest


-- | Request compiler.
compiler :: RequestFieldSetter
compiler r = compose $ fmap (\x -> x r)
   [ setRemote
   , setMethod
   , setPath
   , setHeaders
   , setParams
   , setPayload
   ]


-- | Sets the request's remote information.
setRemote :: RequestFieldSetter
setRemote r = HS.setRequestHost h' . HS.setRequestPort p' . HS.setRequestSecure s'
  where
    r' = IT.requestRemote r
    h' = TE.encodeUtf8 $ IT.remoteHost r'
    p' = fromMaybe (if IT.remoteSecure r' then 443 else 80) $ IT.remotePort r'
    s' = IT.remoteSecure r'


-- | Sets the request method.
setMethod :: RequestFieldSetter
setMethod = HS.setRequestMethod . BC.pack . show . IT.requestHttpMethod


-- | Sets the request path.
setPath :: RequestFieldSetter
setPath r = HS.setRequestPath c
  where
    a = (<>) <$> IT.requestNamespace <*> IT.requestHttpPath $ r
    b = (<>) "/" . B.intercalate "/" . fmap TE.encodeUtf8 . IT.unPath $ a
    t = if IT.requestHttpTrailingSlash r then (<> "/") else id
    c = t b


-- | Sets request headers.
setHeaders :: RequestFieldSetter
setHeaders r = HS.setRequestHeaders h''
  where
    a = ("Authorization", mkAuthorization . IT.requestCredentials $ r)
    u = ("UserAgent", TE.encodeUtf8 . IT.requestUserAgent $ r)
    p =  foldMap (\x -> [("Content-Type", TE.encodeUtf8 . IT.payloadType $ x)]) (IT.requestHttpPayload r)
    h' = fmap (\(x, y) -> (CI.mk $ TE.encodeUtf8 x, TE.encodeUtf8 y)) . IT.requestHttpHeaders $ r
    h'' = a : u : (p <> h')


-- | Sets request querystring parameters.
setParams :: RequestFieldSetter
setParams = HS.setRequestQueryString . fmap (bimap TE.encodeUtf8 (Just . TE.encodeUtf8)). IT.requestHttpParams


-- | Sets request payload.
setPayload :: RequestFieldSetter
setPayload r = maybe id (HS.setRequestBody . H.RequestBodyLBS . IT.payloadContent) $ IT.requestHttpPayload r


-- | Builds an HTTP @Authorization@ header value from given 'IT.Credentials'.
--
-- >>> mkAuthorization $ IT.HeaderCredentials "Token XYZ"
-- "Token XYZ"
-- >>> mkAuthorization $ IT.BasicCredentials "username" "password"
-- "Basic dXNlcm5hbWU6cGFzc3dvcmQ="
-- >>> mkAuthorization $ IT.KeyCredentials "key" "secret"
-- "Key key:secret"
-- >>> mkAuthorization $ IT.TokenCredentials "token"
-- "Token token"
mkAuthorization :: IT.Credentials -> B.ByteString
mkAuthorization (IT.HeaderCredentials x)  = TE.encodeUtf8 x
mkAuthorization (IT.BasicCredentials u p) = mkBasicAuth (TE.encodeUtf8 u) (TE.encodeUtf8 p)
mkAuthorization (IT.KeyCredentials k v)   = mkKeyAuth (TE.encodeUtf8 k) (TE.encodeUtf8 v)
mkAuthorization (IT.TokenCredentials t)   = mkTokenAuth (TE.encodeUtf8 t)


-- | Builds an HTTP @Authorization@ header value from username and password (HTTP Basic Auth).
--
-- >>> mkBasicAuth "username" "password"
-- "Basic dXNlcm5hbWU6cGFzc3dvcmQ="
mkBasicAuth :: B.ByteString -> B.ByteString -> B.ByteString
mkBasicAuth u p = B.append "Basic " $ encode (B.concat [u, ":", p])


-- | Builds an HTTP @Authorization@ header value from DECAF API key and secret.
--
-- >>> mkKeyAuth "key" "secret"
-- "Key key:secret"
mkKeyAuth :: B.ByteString -> B.ByteString -> B.ByteString
mkKeyAuth k s = B.concat ["Key ", k, ":", s]


-- | Builds an HTTP @Authorization@ header value from DECAF API token.
--
-- >>> mkTokenAuth "token"
-- "Token token"
mkTokenAuth :: B.ByteString -> B.ByteString
mkTokenAuth = B.append "Token "


------------------
-- END INTERNAL --
------------------
