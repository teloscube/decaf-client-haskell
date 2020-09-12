{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}

module Decaf.Client.Internal.Http where

import           Data.Aeson            (FromJSON, ToJSON)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.List             (intercalate)
import           Decaf.Client.Version  (version)
import qualified Network.HTTP.Client   as H
import           Network.HTTP.Simple
                 ( Header
                 , Request
                 , addRequestHeader
                 , getResponseBody
                 , httpBS
                 , httpJSON
                 , parseRequest_
                 , setRequestBodyJSON
                 , setRequestMethod
                 , setRequestPath
                 , setRequestQueryString
                 )
import           Text.Printf           (printf)


-- | Type definition for base URL value.
type BaseUrl = String


-- | Type definition for HTTP @Authorization@ header value.
type Authorization = B.ByteString


-- | Type definition for path segment pointing to endpoints.
data Endpoint = Endpoint Method Path


-- | Available endpoint methods.
data Method = GET | POST | PUT | DELETE deriving (Show)


-- | Type definition for path segments pointing to endpoints.
data Path = Path String | PathSegments [String] | TSPath String | TSPathSegments [String]


-- | Type definition for query string parameter.
type Param = (B.ByteString, B.ByteString)


-- | Type definition for query string parameters.
type Params = [Param]


-- | Data definition for request bodies.
data Body a = BSBody ContentType B.ByteString | ToJSON a => JsonBody a


-- | Type definition for @Content-Type@ HTTP header value.
type ContentType = B.ByteString


-- | Type definition for DECAF API requests.
newtype DecafRequest = MkDecafRequest { unDecafRequest :: Request } deriving (Show)


-- | Safe constructor to create 'DecafRequest' values.
--
-- >>> mkDecafRequest "http://example.com" "Token XYZ" []
-- MkDecafRequest {unDecafRequest = Request {
--   host                 = "localhost"
--   port                 = 80
--   secure               = False
--   requestHeaders       = [("Authorization","<REDACTED>"),("User-Agent","DECAF API Client/0.0.0.1 (Haskell)")]
--   path                 = "http://example.com"
--   queryString          = ""
--   method               = "GET"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
-- }
-- }
mkDecafRequest :: BaseUrl -> Authorization -> [Header] -> DecafRequest
mkDecafRequest baseUrl auth headers =  addHeaders' $ baseRequest
  where
    baseRequest = MkDecafRequest $ parseRequest_ ("POST " ++ baseUrl)
    addHeaders' = addHeaders $ ("Authorization", auth) : ("User-Agent", BC.pack userAgent) : headers


-- | Sets the endpoint of a DECAF Client.
--
-- >>> setEndpoint (Endpoint POST $ TSPath "/api/upload") $ mkDecafRequest "http://example.com" "Token XYZ" []
-- MkDecafRequest {unDecafRequest = Request {
--   host                 = "localhost"
--   port                 = 80
--   secure               = False
--   requestHeaders       = [("Authorization","<REDACTED>"),("User-Agent","DECAF API Client/0.0.0.1 (Haskell)")]
--   path                 = "http://example.com/api/upload/"
--   queryString          = ""
--   method               = "POST"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
-- }
-- }
setEndpoint :: Endpoint -> DecafRequest -> DecafRequest
setEndpoint (Endpoint method path) = setMethod method . appendPath path


-- | Sets the HTTP VERB of the 'DecafRequest'.
--
-- >>> setMethod POST $ mkDecafRequest "http://example.com" "Token XYZ" []
-- MkDecafRequest {unDecafRequest = Request {
--   host                 = "localhost"
--   port                 = 80
--   secure               = False
--   requestHeaders       = [("Authorization","<REDACTED>"),("User-Agent","DECAF API Client/0.0.0.1 (Haskell)")]
--   path                 = "http://example.com"
--   queryString          = ""
--   method               = "POST"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
-- }
-- }
setMethod :: Method -> DecafRequest -> DecafRequest
setMethod method (MkDecafRequest request)= MkDecafRequest $ (setRequestMethod (BC.pack $ show method)) request


-- | Appends given path to an 'DecafRequest' value and return a new 'DecafRequest' value.
--
-- >>> appendPath (TSPath "api/version") $ mkDecafRequest "http://example.com" "Token XYZ" []
-- MkDecafRequest {unDecafRequest = Request {
--   host                 = "localhost"
--   port                 = 80
--   secure               = False
--   requestHeaders       = [("Authorization","<REDACTED>"),("User-Agent","DECAF API Client/0.0.0.1 (Haskell)")]
--   path                 = "http://example.com/api/version/"
--   queryString          = ""
--   method               = "GET"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
-- }
-- }
appendPath :: Path -> DecafRequest -> DecafRequest
appendPath path (MkDecafRequest request) = addPath' request
  where
    basePath' = buildUrl (BC.unpack $ H.path request) path
    setPath' = setRequestPath . BC.pack $ basePath'
    addPath' = MkDecafRequest . setPath'


-- | Builds the remote URL.
--
-- >>> buildUrl "http://example" $ TSPath "/api//version/"
-- "http://example/api/version/"
-- >>> buildUrl "http://example" $ TSPathSegments ["/api/", "///", "/", "/version/"]
-- "http://example/api/version/"
-- >>> buildUrl "http://example" $ Path ""
-- "http://example"
-- >>> buildUrl "http://example" $ Path "/"
-- "http://example"
-- >>> buildUrl "http://example" $ PathSegments []
-- "http://example"
-- >>> buildUrl "http://example" $ PathSegments [""]
-- "http://example"
-- >>> buildUrl "http://example" $ PathSegments ["/"]
-- "http://example"
buildUrl :: BaseUrl -> Path -> String
buildUrl baseUrl (Path path) = buildUrl baseUrl $ PathSegments $ splitWhen (== '/') path
buildUrl baseUrl (PathSegments segments) = (intercalate "/" $ baseUrl : (concat $ fmap (splitWhen (== '/')) segments))
buildUrl baseUrl (TSPath path) = buildUrl baseUrl $ TSPathSegments $ splitWhen (== '/') path
buildUrl baseUrl (TSPathSegments segments) = (buildUrl baseUrl $ PathSegments segments) ++ "/"


-- | Splits the @String@ by the given predicate.
--
-- >>> splitWhen (== '/') ""
-- []
-- >>> splitWhen (== '/') "a"
-- ["a"]
-- >>> splitWhen (== '/') "/a"
-- ["a"]
-- >>> splitWhen (== '/') "a/"
-- ["a"]
-- >>> splitWhen (== '/') "a/b"
-- ["a","b"]
-- >>> splitWhen (== '/') "a//b//c"
-- ["a","b","c"]
splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen p s =  case dropWhile p s of
  "" -> []
  s' -> w : splitWhen p s''
    where (w, s'') = break p s'


-- | User agent value definition for the library.
--
-- >>> userAgent
-- "DECAF API Client/0.0.0.1 (Haskell)"
userAgent :: String
userAgent = printf "DECAF API Client/%s (Haskell)" version


-- | Adds headers to the request.
--
-- >>> addHeaders [("X-A", "1"), ("X-B", "2"), ("X-C", "3")] $ parseRequest_ "http://example"
-- Request {
--   host                 = "example"
--   port                 = 80
--   secure               = False
--   requestHeaders       = [("X-C","3"),("X-B","2"),("X-A","1")]
--   path                 = "/"
--   queryString          = ""
--   method               = "GET"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
-- }
addHeaders :: [Header] -> DecafRequest -> DecafRequest
addHeaders headers decafRequest@(MkDecafRequest request) = case headers of
  []            -> decafRequest
  (hn, hv) : hs -> addHeaders hs (MkDecafRequest (addRequestHeader hn hv request))


-- | Add query string parameters to a 'DecafRequest'.
--
-- >>> addParams [("a", "1"), ("b", "2"), ("b", "3")] $ mkDecafRequest "http://example.com" "Token XYZ" []
-- Request {
--   host                 = "example"
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
addParams :: Params -> DecafRequest -> DecafRequest
addParams params (MkDecafRequest request) = MkDecafRequest $ setRequestQueryString (fmap (\(x, y) -> (x, Just y)) params) request


-- | Adds JSON body to a 'DecafRequest'
addJsonBody :: ToJSON a => a -> DecafRequest -> DecafRequest
addJsonBody body (MkDecafRequest request) = MkDecafRequest $ setRequestBodyJSON body request


-- | Attempts to perform a 'DecafRequest' that returns 'B.ByteString'.
performRequestBS :: DecafRequest -> IO B.ByteString
performRequestBS (MkDecafRequest request) = getResponseBody <$> httpBS request


-- | Attempts to perform a 'DecafRequest' that returns a value decoded from a JSON response body.
performRequestJson :: FromJSON a => DecafRequest -> IO a
performRequestJson (MkDecafRequest request) = getResponseBody <$> httpJSON request
