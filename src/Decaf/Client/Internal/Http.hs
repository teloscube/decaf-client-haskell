-- | This module provides machinery to abstract over the underlying
-- "http-conduit" library. Definitions in this module are internal and not
-- exported on "Decaf.Client".
--
-- __TODO:__ Choose a proper monad to work with, handle various HTTP return
-- codes for errors.

module Decaf.Client.Internal.Http where

import           Control.Monad.Catch           (MonadCatch, MonadThrow)
import           Control.Monad.IO.Class        (MonadIO)
import qualified Data.Aeson.Types              as Aeson
import qualified Data.ByteString               as B
import           Data.ByteString.Base64        (encode)
import qualified Data.ByteString.Char8         as BC
import qualified Data.ByteString.Lazy          as BL
import           Data.Maybe                    (fromMaybe)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           Decaf.Client.DecafCredentials
                 ( DecafBasicCredentials(DecafBasicCredentials)
                 , DecafCredentials(..)
                 , DecafKeyCredentials(DecafKeyCredentials)
                 )
import           Decaf.Client.DecafRemote      (DecafRemote(decafRemoteHost, decafRemotePort, decafRemoteSecure))
import           Decaf.Client.DecafRequest     (DecafRequest(..), DecafRequestPayload(..), unDecafRequestPath)
import           Decaf.Client.DecafResponse    (DecafResponse(DecafResponse, decafResponseBody))
import           Decaf.Client.Internal.Utils   (compose)
import qualified Network.HTTP.Client.Conduit   as HC
import qualified Network.HTTP.Simple           as HS
import           Network.HTTP.Types            (queryTextToQuery)


-- * HTTP Request Runners
-- $httpRequestRunners


runRequestBS
  :: MonadIO m
  => MonadCatch m
  => MonadThrow m
  => DecafRequest
  -> m (DecafResponse B.ByteString)
runRequestBS r = mkResponse <$> (HS.httpBS . compileRequest) r


runRequestBL
  :: MonadIO m
  => MonadCatch m
  => MonadThrow m
  => DecafRequest
  -> m (DecafResponse BL.ByteString)
runRequestBL r = mkResponse <$> (HS.httpLBS . compileRequest) r


runRequestText
  :: MonadIO m
  => MonadCatch m
  => MonadThrow m
  => DecafRequest
  -> m (DecafResponse T.Text)
runRequestText = fmap (\x -> x { decafResponseBody = TE.decodeUtf8 (decafResponseBody x) } ) . runRequestBS


runRequestJson
  :: Aeson.FromJSON a
  => MonadIO m
  => MonadCatch m
  => MonadThrow m
  => DecafRequest
  -> m (DecafResponse a)
runRequestJson r = mkResponse <$> (HS.httpJSON . compileRequest) r


runRequestVoid
  :: MonadIO m
  => MonadCatch m
  => MonadThrow m
  => DecafRequest
  -> m (DecafResponse ())
runRequestVoid r = mkResponse <$> (HS.httpNoBody . compileRequest) r


-- * Internal
-- $internal


-- | Converts the underlying "http-conduit" 'HS.Response' value into a DECAF
-- client 'Response' value.
mkResponse :: HS.Response a -> DecafResponse a
mkResponse = DecafResponse
  <$> HS.getResponseStatus
  <*> HS.getResponseHeaders
  <*> HS.getResponseBody


-- | Type definition to modify "http-conduit" 'H.Request' fields from a given
-- DECAF client 'DecafRequest' value.
type RequestFieldSetter = DecafRequest -> HS.Request -> HS.Request


-- | Compiles a DECAF client 'DecafRequest' into a "http-conduit" 'H.Request'.
compileRequest :: DecafRequest -> HS.Request
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
    r' = decafRequestRemote r
    h' = TE.encodeUtf8 $ decafRemoteHost r'
    p' = fromMaybe (if decafRemoteSecure r' then 443 else 80) $ decafRemotePort r'
    s' = decafRemoteSecure r'


-- | Sets the request method.
setMethod :: RequestFieldSetter
setMethod = HS.setRequestMethod . BC.pack . show . decafRequestMethod


-- | Sets the request path.
setPath :: RequestFieldSetter
setPath r = HS.setRequestPath c
  where
    a = (<>) <$> decafRequestNamespace <*> decafRequestPath $ r
    b = (<>) "/" . B.intercalate "/" . fmap TE.encodeUtf8 . unDecafRequestPath $ a
    t = if decafRequestTrailingSlash r then (<> "/") else id
    c = t b


-- | Sets request headers.
setHeaders :: RequestFieldSetter
setHeaders r = HS.setRequestHeaders h''
  where
    a = ("Authorization", mkAuthorization . decafRequestCredentials $ r)
    u = ("UserAgent", TE.encodeUtf8 . decafRequestUserAgent $ r)
    p =  foldMap (\x -> [("Content-Type", TE.encodeUtf8 . decafRequestPayloadType $ x)]) (decafRequestPayload r)
    h' = decafRequestHeaders r
    h'' = a : u : (p <> h')


-- | Sets request querystring parameters.
setParams :: RequestFieldSetter
setParams = HS.setRequestQueryString . queryTextToQuery . decafRequestQuery


-- | Sets request payload.
setPayload :: RequestFieldSetter
setPayload r = maybe id (HS.setRequestBody . HC.RequestBodyLBS . decafRequestPayloadContent) $ decafRequestPayload r


-- | Builds an HTTP @Authorization@ header value from given 'Credentials'.
--
-- >>> import Decaf.Client.DecafCredentials
-- >>> mkAuthorization $ DecafCredentialsHeader "Token XYZ"
-- "Token XYZ"
-- >>> mkAuthorization $ DecafCredentialsBasic (DecafBasicCredentials "username" "password")
-- "Basic dXNlcm5hbWU6cGFzc3dvcmQ="
-- >>> mkAuthorization $ DecafCredentialsKey (DecafKeyCredentials "key" "secret")
-- "Key key:secret"
-- >>> mkAuthorization $ DecafCredentialsToken "token"
-- "Token token"
mkAuthorization :: DecafCredentials -> B.ByteString
mkAuthorization (DecafCredentialsHeader x)                          = TE.encodeUtf8 x
mkAuthorization (DecafCredentialsBasic (DecafBasicCredentials u p)) = mkBasicAuth (TE.encodeUtf8 u) (TE.encodeUtf8 p)
mkAuthorization (DecafCredentialsKey (DecafKeyCredentials k v))     = mkKeyAuth (TE.encodeUtf8 k) (TE.encodeUtf8 v)
mkAuthorization (DecafCredentialsToken t)                           = mkTokenAuth (TE.encodeUtf8 t)


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
