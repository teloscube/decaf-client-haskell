-- | This module provides machinery to abstract over the underlying
-- "http-conduit" library. Definitions in this module are internal and not
-- exported on "Decaf.Client".
--
-- __TODO:__ Choose a proper monad to work with, handle various HTTP return
-- codes for errors.

module Decaf.Client.Internal.Http where

import           Control.Monad.IO.Class            (MonadIO)
import           Data.Aeson                        (FromJSON)
import           Data.Bifunctor                    (bimap)
import qualified Data.ByteString                   as B
import           Data.ByteString.Base64            (encode)
import qualified Data.ByteString.Char8             as BC
import qualified Data.CaseInsensitive              as CI
import           Data.Maybe                        (fromMaybe)
import qualified Data.Text.Encoding                as TE
import           Decaf.Client.Internal.Credentials
                 ( BasicCredentials(BasicCredentials)
                 , Credentials(..)
                 , KeyCredentials(KeyCredentials)
                 )
import           Decaf.Client.Internal.Remote      (Remote(remoteHost, remotePort, remoteSecure))
import           Decaf.Client.Internal.Request     (Payload(..), Request(..), unPath)
import           Decaf.Client.Internal.Response    (Response(Response))
import           Decaf.Client.Internal.Utils       (compose)
import qualified Network.HTTP.Client.Conduit       as HC
import qualified Network.HTTP.Simple               as HS


-- * HTTP Request Runners
-- $httpRequestRunners

-- | Runs a request and returns a 'Response' value JSON-decoded from the
-- response body.
runRequest :: (MonadIO m, FromJSON a) => Request -> m (Response a)
runRequest r = mkResponse <$> (HS.httpJSON . compileRequest) r


-- | Runs a request and returns a 'Response' with 'B.ByteString' value.
runRequestBS :: MonadIO m => Request -> m (Response B.ByteString)
runRequestBS r = mkResponse <$> (HS.httpBS . compileRequest) r


-- * Internal
-- $internal


-- | Converts the underlying "http-conduit" 'HS.Response' value into a DECAF
-- client 'Response' value.
mkResponse :: HS.Response a -> Response a
mkResponse = Response
  <$> HS.getResponseStatusCode
  <*> (fmap (bimap (TE.decodeUtf8 . CI.foldedCase) TE.decodeUtf8) . HS.getResponseHeaders)
  <*> HS.getResponseBody


-- | Type definition to modify "http-conduit" 'H.Request' fields from a given
-- DECAF client 'Request' value.
type RequestFieldSetter = Request -> HS.Request -> HS.Request


-- | Compiles a DECAF client 'Request' into a "http-conduit" 'H.Request'.
compileRequest :: Request -> HS.Request
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
    r' = requestRemote r
    h' = TE.encodeUtf8 $ remoteHost r'
    p' = fromMaybe (if remoteSecure r' then 443 else 80) $ remotePort r'
    s' = remoteSecure r'


-- | Sets the request method.
setMethod :: RequestFieldSetter
setMethod = HS.setRequestMethod . BC.pack . show . requestHttpMethod


-- | Sets the request path.
setPath :: RequestFieldSetter
setPath r = HS.setRequestPath c
  where
    a = (<>) <$> requestNamespace <*> requestHttpPath $ r
    b = (<>) "/" . B.intercalate "/" . fmap TE.encodeUtf8 . unPath $ a
    t = if requestHttpTrailingSlash r then (<> "/") else id
    c = t b


-- | Sets request headers.
setHeaders :: RequestFieldSetter
setHeaders r = HS.setRequestHeaders h''
  where
    a = ("Authorization", mkAuthorization . requestCredentials $ r)
    u = ("UserAgent", TE.encodeUtf8 . requestUserAgent $ r)
    p =  foldMap (\x -> [("Content-Type", TE.encodeUtf8 . payloadType $ x)]) (requestHttpPayload r)
    h' = fmap (\(x, y) -> (CI.mk $ TE.encodeUtf8 x, TE.encodeUtf8 y)) . requestHttpHeaders $ r
    h'' = a : u : (p <> h')


-- | Sets request querystring parameters.
setParams :: RequestFieldSetter
setParams = HS.setRequestQueryString . fmap (bimap TE.encodeUtf8 (Just . TE.encodeUtf8)). requestHttpParams


-- | Sets request payload.
setPayload :: RequestFieldSetter
setPayload r = maybe id (HS.setRequestBody . HC.RequestBodyLBS . payloadContent) $ requestHttpPayload r


-- | Builds an HTTP @Authorization@ header value from given 'Credentials'.
--
-- >>> import Decaf.Client.Internal.Credentials
-- >>> mkAuthorization $ CredentialsHeader "Token XYZ"
-- "Token XYZ"
-- >>> mkAuthorization $ CredentialsBasic (BasicCredentials "username" "password")
-- "Basic dXNlcm5hbWU6cGFzc3dvcmQ="
-- >>> mkAuthorization $ CredentialsKey (KeyCredentials "key" "secret")
-- "Key key:secret"
-- >>> mkAuthorization $ CredentialsToken "token"
-- "Token token"
mkAuthorization :: Credentials -> B.ByteString
mkAuthorization (CredentialsHeader x)                     = TE.encodeUtf8 x
mkAuthorization (CredentialsBasic (BasicCredentials u p)) = mkBasicAuth (TE.encodeUtf8 u) (TE.encodeUtf8 p)
mkAuthorization (CredentialsKey (KeyCredentials k v))     = mkKeyAuth (TE.encodeUtf8 k) (TE.encodeUtf8 v)
mkAuthorization (CredentialsToken t)                      = mkTokenAuth (TE.encodeUtf8 t)


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
