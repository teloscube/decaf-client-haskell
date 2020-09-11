{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}

module Decaf.Client.Http.Utils where

import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as BC
import           Data.List                   (intercalate)
import           Decaf.Client.Version        (version)
import           Network.HTTP.Client.Conduit (RequestBody(RequestBodyBS))
import           Network.HTTP.Simple
                 ( Header
                 , Request
                 , addRequestHeader
                 , getResponseBody
                 , httpBS
                 , httpJSON
                 , setRequestBody
                 , setRequestBodyJSON
                 , setRequestHeader
                 , setRequestQueryString
                 )
import           Network.HTTP.Simple         (parseRequest_)
import           Network.HTTP.Types.Header   (HeaderName)
import           Text.Printf                 (printf)


-- | Type definition for HTTP @Authorization@ header value.
type Authorization = B.ByteString


-- | Type definition for base URL value.
type BaseUrl = String


-- | Type definition for query parameters.
type Params = [(B.ByteString, B.ByteString)]


-- | Type definition for @Content-Type@ HTTP header value.
type ContentType = B.ByteString


-- | Type definition for path segment pointing to endpoints.
data Endpoint = Endpoint String | EndpointPath [String] | TSEndpoint String | TSEndpointPath [String]


-- | Available methods.
data Method = GET | POST | PUT | DELETE deriving (Show)


-- | Data definition for request bodies.
data Body a = BSBody ContentType B.ByteString | ToJSON a => JsonBody a


-- | Data definition for a generic DECAF API client request.
data DecafRequest = DecafRequest
  { decafRequestDeploymentUrl :: !BaseUrl
  , decafRequestAuthorization :: !Authorization
  , decafRequestHeaders       :: ![Header]
  , decafRequestMethod        :: !Method
  , decafRequestEndpoint      :: !Endpoint
  }


-- | Type definition for @DecafRequest@ to @Request@ compiler.
type RequestCompiler = DecafRequest -> Request


-- | Attempts to consume the remote endpoint with the given @DecafRequest@ and a request compiler.
requestBS :: DecafRequest -> RequestCompiler -> IO B.ByteString
requestBS request compiler = getResponseBody <$> httpBS (compiler request)


-- | Attempts to consume the remote endpoint with the given @DecafRequest@ and a request compiler, and returns a value.
requestJson :: FromJSON a => RequestCompiler -> DecafRequest -> IO a
requestJson requestCompiler decafRequest = getResponseBody <$> httpJSON (requestCompiler decafRequest)


-- | Prepare the request.
--
-- >>> prepareRequest $ DecafRequest "http://example.com" "Token XYZ" [("X-Header", "1")] GET (TSEndpoint "/api/version/")
-- Request {
--   host                 = "example.com"
--   port                 = 80
--   secure               = False
--   requestHeaders       = [("X-Header","1"),("User-Agent","DECAF API Client/0.0.0.1 (Haskell)"),("Authorization","<REDACTED>")]
--   path                 = "/api/version/"
--   queryString          = ""
--   method               = "GET"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
-- }
prepareRequest :: DecafRequest -> Request
prepareRequest req@(DecafRequest _ auth headers _ _) =  makeRequest req
  where
    makeRequest = (addHeaders headers . authorizer . addAgent . parseRequest_ . buildBaseRequestUrl)
    authorizer = addAuthorization auth


prepareRequestWithParams :: Params -> DecafRequest -> Request
prepareRequestWithParams params = addParams params . prepareRequest


prepareRequestWithBody :: Body a -> DecafRequest -> Request
prepareRequestWithBody (BSBody contentType body) = addRequestHeader "Content-Type" contentType . setRequestBody (RequestBodyBS body) . prepareRequest
prepareRequestWithBody (JsonBody body) = setRequestBodyJSON body . prepareRequest


prepareRequestWithParamsAndBody :: Params -> Body a -> DecafRequest -> Request
prepareRequestWithParamsAndBody params body = addParams params . prepareRequestWithBody body


-- | Build request method + URL specification.
--
-- >>> buildBaseRequestUrl $ DecafRequest "http://example.com" "Token XYZ" [] GET (Endpoint "/api/version/")
-- "GET http://example.com/api/version"
buildBaseRequestUrl :: DecafRequest -> String
buildBaseRequestUrl (DecafRequest baseUrl _ _ method endpoint) = printf "%s %s" (show method) $ buildUrl baseUrl endpoint


-- | Builds the remote URL.
--
-- >>> buildUrl "http://example" $ TSEndpoint "/api//version/"
-- "http://example/api/version/"
-- >>> buildUrl "http://example" $ TSEndpointPath ["/api/", "///", "/", "/version/"]
-- "http://example/api/version/"
-- >>> buildUrl "http://example" $ Endpoint ""
-- "http://example"
-- >>> buildUrl "http://example" $ Endpoint "/"
-- "http://example"
-- >>> buildUrl "http://example" $ EndpointPath []
-- "http://example"
-- >>> buildUrl "http://example" $ EndpointPath [""]
-- "http://example"
-- >>> buildUrl "http://example" $ EndpointPath ["/"]
-- "http://example"
buildUrl :: BaseUrl -> Endpoint -> String
buildUrl baseUrl (Endpoint endpoint) = buildUrl baseUrl $ EndpointPath $ splitWhen (== '/') endpoint
buildUrl baseUrl (EndpointPath path) = (intercalate "/" $ baseUrl : (concat $ fmap (splitWhen (== '/')) path))
buildUrl baseUrl (TSEndpoint endpoint) = buildUrl baseUrl $ TSEndpointPath $ splitWhen (== '/') endpoint
buildUrl baseUrl (TSEndpointPath path) = (intercalate "/" $ baseUrl : (concat $ fmap (splitWhen (== '/')) path)) ++ "/"


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


-- | Adds @Authorization@ header to the request.
--
-- >>> addAuthorization "Token XYZ" $ parseRequest_ "http://example"
-- Request {
--   host                 = "example"
--   port                 = 80
--   secure               = False
--   requestHeaders       = [("Authorization","<REDACTED>")]
--   path                 = "/"
--   queryString          = ""
--   method               = "GET"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
-- }
addAuthorization :: B.ByteString -> Request -> Request
addAuthorization auth = setRequestHeader "Authorization" [auth]

-- | Adds custom @User-Agent@ header to the request.
--
-- >>> addAgent $ parseRequest_ "http://example"
-- Request {
--   host                 = "example"
--   port                 = 80
--   secure               = False
--   requestHeaders       = [("User-Agent","DECAF API Client/0.0.0.1 (Haskell)")]
--   path                 = "/"
--   queryString          = ""
--   method               = "GET"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
-- }
addAgent :: Request -> Request
addAgent = setRequestHeader "User-Agent" [BC.pack $ printf "DECAF API Client/%s (Haskell)" version]


-- | Add query string parameters to the request.
--
-- >>> addParams [("a", "1"), ("b", "2"), ("b", "3")] $ parseRequest_ "http://example"
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
addParams :: Params -> Request -> Request
addParams params = setRequestQueryString $ fmap (\(x, y) -> (x, Just y)) params


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
addHeaders :: [(HeaderName, B.ByteString)] -> Request -> Request
addHeaders pHeaders pRequest = case pHeaders of
  []            -> pRequest
  (hn, hv) : hs -> addHeaders hs (addRequestHeader hn hv pRequest)
