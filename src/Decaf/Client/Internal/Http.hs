-- | This module provides types and functions to abstract over underlying HTTP
-- client implementation.
{-# LANGUAGE OverloadedStrings #-}

module Decaf.Client.Internal.Http where

import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.ByteString             as B
import           Data.ByteString.Base64      (encode)
import qualified Data.ByteString.Char8       as BC
import           Data.List                   (intercalate)
import           Decaf.Client.Internal.Utils (removeTrailingChar, splitWhen)
import           Decaf.Client.Version        (version)
import qualified Network.HTTP.Client         as H
import qualified Network.HTTP.Simple         as HS
import           Text.Printf                 (printf)


-- | Type definition for DECAF API requests.
newtype Request = MkRequest { unRequest :: HttpRequest } deriving (Show)


-- | Builds an initial 'Request' value.
--
-- >>> mkRequest (mkBaseUrl "http://example.com") (mkAuthorization (HeaderCredentials "Token XYZ")) []
-- MkRequest {unRequest = Request {
--   host                 = "example.com"
--   port                 = 80
--   secure               = False
--   requestHeaders       = [("Authorization","<REDACTED>"),("User-Agent","DECAF API Client/0.0.0.1 (Haskell)")]
--   path                 = "/"
--   queryString          = ""
--   method               = "POST"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
-- }
-- }
mkRequest :: BaseUrl -> Authorization -> [HS.Header] -> Request
mkRequest (MkBaseUrl baseUrl) (MkAuthorization auth) headers =  (MkRequest . addHeaders') baseRequest
  where
    baseRequest = HS.parseRequest_ ("POST " ++ baseUrl)
    addHeaders' = addHttpHeaders $ ("Authorization", auth) : ("User-Agent", BC.pack userAgent) : headers


-- | Type definition for HTTP urls.
type Url = String

-- | Type definition for base URLs.
--
-- Values should not have trailing slashes.
newtype BaseUrl = MkBaseUrl Url deriving (Show)


-- | Function to construct 'BaseUrl' values.
--
-- >>> mkBaseUrl "http://example.com"
-- MkBaseUrl "http://example.com"
-- >>> mkBaseUrl "http://example.com/"
-- MkBaseUrl "http://example.com"
-- >>> mkBaseUrl "http://example.com//"
-- MkBaseUrl "http://example.com"
mkBaseUrl :: Url -> BaseUrl
mkBaseUrl = MkBaseUrl . removeTrailingChar '/'


-- | Type definition for path segments pointing to endpoints.
--
-- Use 'mkPath' function to sanitise path segments argument.
data Path = MkPath [String] | MkPathWTS [String] deriving (Show)


-- | Builds a 'Path'.
--
-- >>> mkPath True ["api", "version"]
-- MkPathWTS ["api","version"]
-- >>> mkPath False ["apis", "microlot", "v1", "graphql"]
-- MkPath ["apis","microlot","v1","graphql"]
-- >>> mkPath False ["//apis//microlot//v1graphql//"]
-- MkPath ["apis","microlot","v1graphql"]
mkPath
  :: Bool      -- ^ Indicates if the HTTP path should have a trailing slash.
  -> [String]  -- ^ Path segments.
  -> Path
mkPath ts = (if ts then MkPathWTS else MkPath) . concatMap (splitWhen (== '/'))


-- | HTTP Header type.
--
-- This is essentially the same as 'Network.HTTP.Simple.Header', but re-exported
-- for convenience purposes.
type Header = HS.Header


-- | Type definitions for a list of HTTP Headers.
type Headers = [Header]


-- | Type definition for query string parameter.
type Param = (B.ByteString, B.ByteString)


-- | Type definition for query string parameters.
type Params = [Param]


-- | Type definitions for HTTP requests.
type HttpRequest = HS.Request


-- | Data definition for available DECAF credentials types.
data Credentials =
    HeaderCredentials B.ByteString
  | BasicCredentials B.ByteString B.ByteString
  | KeyCredentials B.ByteString B.ByteString
  | TokenCredentials B.ByteString


-- | Type definition for HTTP @Authorization@ header value for remote DECAF APIs.
newtype Authorization = MkAuthorization B.ByteString deriving (Show)


-- | Builds a 'Authorization' value from given 'Credentials'
--
-- >>> mkAuthorization $ HeaderCredentials "Token XYZ"
-- MkAuthorization "Token XYZ"
-- >>> mkAuthorization $ BasicCredentials "username" "password"
-- MkAuthorization "Basic dXNlcm5hbWU6cGFzc3dvcmQ="
-- >>> mkAuthorization $ KeyCredentials "key" "secret"
-- MkAuthorization "Key key:secret"
-- >>> mkAuthorization $ TokenCredentials "token"
-- MkAuthorization "Token token"
mkAuthorization :: Credentials -> Authorization
mkAuthorization (HeaderCredentials x)  = MkAuthorization x
mkAuthorization (BasicCredentials u p) = MkAuthorization $ mkBasicAuth u p
mkAuthorization (KeyCredentials k v)   = MkAuthorization $ mkKeyAuth k v
mkAuthorization (TokenCredentials t)   = MkAuthorization $ mkTokenAuth t


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


-- | Builds URL from given 'Url' and 'Path' values.
--
-- >>> mkUrl "http://example.com" (mkPath False [])
-- "http://example.com"
-- >>> mkUrl "http://example.com/" (mkPath False [])
-- "http://example.com"
-- >>> mkUrl "http://example.com" (mkPath True [])
-- "http://example.com/"
-- >>> mkUrl "http://example.com/" (mkPath True [])
-- "http://example.com/"
-- >>> mkUrl "http://example.com" (mkPath False ["a"])
-- "http://example.com/a"
-- >>> mkUrl "http://example.com/" (mkPath False ["a"])
-- "http://example.com/a"
-- >>> mkUrl "http://example.com" (mkPath True ["a"])
-- "http://example.com/a/"
-- >>> mkUrl "http://example.com/" (mkPath True ["a"])
-- "http://example.com/a/"
-- >>> mkUrl "http://example.com" (mkPath True ["api", "version"])
-- "http://example.com/api/version/"
-- >>> mkUrl "http://example.com/" (mkPath True ["api", "version"])
-- "http://example.com/api/version/"
-- >>> mkUrl "http://example.com" (mkPath False ["apis", "microlot", "v1", "graphql"])
-- "http://example.com/apis/microlot/v1/graphql"
-- >>> mkUrl "http://example.com/" (mkPath False ["apis", "microlot", "v1", "graphql"])
-- "http://example.com/apis/microlot/v1/graphql"
mkUrl :: Url -> Path -> Url
mkUrl base (MkPath path)    = intercalate "/" $ removeTrailingChar '/' base : path
mkUrl base (MkPathWTS path) = mkUrl base (MkPath path) ++ "/"


-- | Content-Type header value type.
type ContentType = String


-- | Type definition for path segment pointing to endpoints.
data Endpoint = Endpoint Method Path


-- | Available DECAF endpoint methods.
data Method = GET | POST | PUT | DELETE deriving (Show)


-- | Sets the endpoint of the 'Request'
setEndpoint :: Endpoint -> Request -> Request
setEndpoint = modifyRequest . setHttpEndpoint


-- | Sets query string parameters of a 'Request'.
--
-- >>> setParams [("a", "1"), ("b", "2"), ("b", "3")] $ initRequest (mkDeployment "http://example.com") (mkAuthorization (HeaderCredentials "Token XYZ")) []
setParams :: Params -> Request -> Request
setParams = modifyRequest . setHttpParams


-- | Adds query string parameters to a 'Request'.
--
-- >>> addParams [("b", "2")] $ addParams [("a", "1")] $ initRequest (mkDeployment "http://example.com") (mkAuthorization (HeaderCredentials "Token XYZ")) []
addParams :: Params -> Request -> Request
addParams = modifyRequest . addHttpParams


-- | Adds headers to a 'Request'.
addHeaders :: Headers -> Request -> Request
addHeaders = modifyRequest . addHttpHeaders


-- | Adds 'Path' to a 'Request'
addPath :: Path -> Request -> Request
addPath = modifyRequest . addHttpPath


-- | Add 'ByteString' payload to 'Request' along with its content-type.
addPayload :: ContentType -> B.ByteString -> Request -> Request
addPayload ctype = modifyRequest . addHttpBody ctype


-- | Add JSON payload to 'Request'.
addJsonPayload :: ToJSON a =>  a -> Request -> Request
addJsonPayload = modifyRequest . addHttpBodyJson


-- | Attempts to perform a 'DecafRequest' that returns 'B.ByteString'.
perform :: Request -> IO B.ByteString
perform (MkRequest request) = HS.getResponseBody <$> HS.httpBS request


-- | Attempts to perform a 'DecafRequest' that returns a value decoded from a JSON response body.
performJson :: FromJSON a => Request -> IO a
performJson (MkRequest request) = HS.getResponseBody <$> HS.httpJSON request


-------------------
-- LOW LEVEL API --
-------------------


-- | Modifies a 'Request'.
modifyRequest :: (HttpRequest -> HttpRequest) -> Request -> Request
modifyRequest modifier = MkRequest . modifier  . unRequest


-- | Sets the endpoint of a DECAF request.
--
-- Given path is appended to the existing path of the given request.
--
-- >>> setHttpEndpoint (Endpoint POST $ mkPath True ["upload"]) HS.defaultRequest
-- Request {
--   host                 = "localhost"
--   port                 = 80
--   secure               = False
--   requestHeaders       = []
--   path                 = "/upload/"
--   queryString          = ""
--   method               = "POST"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
-- }
setHttpEndpoint :: Endpoint -> HttpRequest -> HttpRequest
setHttpEndpoint (Endpoint method path) = setHttpMethod method . addHttpPath path


-- | Sets the HTTP VERB of the given 'HttpRequest'.
--
-- >>> setHttpMethod POST HS.defaultRequest
-- Request {
--   host                 = "localhost"
--   port                 = 80
--   secure               = False
--   requestHeaders       = []
--   path                 = "/"
--   queryString          = ""
--   method               = "POST"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
-- }
setHttpMethod :: Method -> HttpRequest -> HttpRequest
setHttpMethod method = HS.setRequestMethod $ (BC.pack . show) method


-- | Adds additional headers to the request.
--
-- If there is a header that is already set, its value will be overridden.
--
-- >>> addHttpHeaders [("X-B", "3"), ("X-C", "4")] $ addHttpHeaders [("X-A", "1"), ("X-B", "2")] $ HS.defaultRequest
-- Request {
--   host                 = "localhost"
--   port                 = 80
--   secure               = False
--   requestHeaders       = [("X-A","1"),("X-B","3"),("X-C","4")]
--   path                 = "/"
--   queryString          = ""
--   method               = "GET"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
-- }
addHttpHeaders :: [Header] -> HttpRequest -> HttpRequest
addHttpHeaders headers request = case headers of
  []            -> request
  (hn, hv) : hs -> addHttpHeaders hs (HS.setRequestHeader hn [hv] request)


-- | Appends given path to an existing value and return a new request value.
--
-- >>> addHttpPath (mkPath True ["api", "version"]) HS.defaultRequest
-- Request {
--   host                 = "localhost"
--   port                 = 80
--   secure               = False
--   requestHeaders       = []
--   path                 = "/api/version/"
--   queryString          = ""
--   method               = "GET"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
-- }
-- >>> addHttpPath (mkPath True ["version"]) $ addHttpPath (mkPath True ["api"]) HS.defaultRequest
-- Request {
--   host                 = "localhost"
--   port                 = 80
--   secure               = False
--   requestHeaders       = []
--   path                 = "/api/version/"
--   queryString          = ""
--   method               = "GET"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
-- }
-- >>> addHttpPath (mkPath False ["v1", "graphql"]) $ addHttpPath (mkPath True ["microlot"]) $ addHttpPath (mkPath True ["apis"]) HS.defaultRequest
-- Request {
--   host                 = "localhost"
--   port                 = 80
--   secure               = False
--   requestHeaders       = []
--   path                 = "/apis/microlot/v1/graphql"
--   queryString          = ""
--   method               = "GET"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
-- }
addHttpPath :: Path -> HttpRequest -> HttpRequest
addHttpPath path request = addPath' request
  where
    basePath' = mkUrl (BC.unpack $ H.path request) path
    setPath' = HS.setRequestPath . BC.pack $ basePath'
    addPath' = setPath'


-- | Sets query string parameters of a 'Request'.
--
-- >>> setHttpParams [("a", "1"), ("b", "2"), ("b", "3")] HS.defaultRequest
-- Request {
--   host                 = "localhost"
--   port                 = 80
--   secure               = False
--   requestHeaders       = []
--   path                 = "/"
--   queryString          = "?a=1&b=2&b=3"
--   method               = "GET"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
-- }
setHttpParams :: Params -> HttpRequest -> HttpRequest
setHttpParams = HS.setRequestQueryString . mkHttpQuery


-- | Adds query string parameters to a 'Request'.
--
-- >>> addHttpParams [("b", "2")] $ addHttpParams [("a", "1")] HS.defaultRequest
-- Request {
--   host                 = "localhost"
--   port                 = 80
--   secure               = False
--   requestHeaders       = []
--   path                 = "/"
--   queryString          = "?b=2&a=1"
--   method               = "GET"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
-- }
addHttpParams :: Params -> HttpRequest -> HttpRequest
addHttpParams = HS.addToRequestQueryString . mkHttpQuery


-- | Builds a 'HS.Query' from 'Params'
--
-- >>> mkHttpQuery [("a", "1"), ("b", "2")]
-- [("a",Just "1"),("b",Just "2")]
mkHttpQuery :: Params -> HS.Query
mkHttpQuery = fmap (\(x, y) -> (x, Just y))


-- | Adds body to an 'HttpRequest'
addHttpBody :: ContentType -> B.ByteString -> HttpRequest -> HttpRequest
addHttpBody ctype body = addHttpHeaders [("Content-Type", BC.pack ctype)] . HS.setRequestBody (H.RequestBodyBS body)


-- | Adds JSON body to an 'HttpRequest'
addHttpBodyJson :: ToJSON a => a -> HttpRequest -> HttpRequest
addHttpBodyJson = HS.setRequestBodyJSON


-- | User agent value definition for the library.
--
-- >>> userAgent
-- "DECAF API Client/0.0.0.1 (Haskell)"
userAgent :: String
userAgent = printf "DECAF API Client/%s (Haskell)" version
