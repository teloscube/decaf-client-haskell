-- | This module provides auxiliaries to build and work with 'Request' values.
--
{-# LANGUAGE OverloadedStrings #-}

module Decaf.Client.Internal.Request where

import qualified Data.Char                         as C
import qualified Data.Text                         as T
import           Decaf.Client.Internal.Combinators (credentials, header, host, port, secure)
import           Decaf.Client.Internal.Types       (Credentials(HeaderCredentials), Method(GET), Request(..))
import           Decaf.Client.Internal.Utils       (dropLeading, dropTrailing, maybeToEither, nonEmptyString)
import           Decaf.Client.Version              (version)
import qualified Network.URI                       as U
import           Text.Printf                       (printf)
import           Text.Read                         (readMaybe)


-- | Initializes a request with deployment URL and authentication credentials.
--
-- >>> initRequest "http://example.com" (HeaderCredentials "OUCH")
-- Right Request {
--   requestHost              = "example.com"
--   requestPort              = Nothing
--   requestNamespace         = MkPath {unPath = []}
--   requestIsSecure          = False
--   requestCredentials       = <********>
--   requestUserAgent         = "DECAF API Client/0.0.0.1 (Haskell)"
--   requestHttpHeaders       = [("X-DECAF-URL","http://example.com")]
--   requestHttpMethod        = GET
--   requestHttpPath          = MkPath {unPath = []}
--   requestHttpTrailingSlash = False
--   requestHttpParams        = []
--   requestHttpPayload       = Nothing
-- }
initRequest :: T.Text -> Credentials -> Either String Request
initRequest deployment c = do
  (h, p, s) <- parseDeployment deployment
  pure $ (host h . port p . secure s . credentials c . header "X-DECAF-URL" deployment) defaultRequest


--------------------
-- BEGIN INTERNAL --
--------------------

-- | Default 'Request'.
--
-- This is useful to build 'Request' values using combinators.
defaultRequest :: Request
defaultRequest = Request
  { requestHost = "localhost"
  , requestPort = Nothing
  , requestNamespace = mempty
  , requestIsSecure = False
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


-- | Parses a deployment URL.
--
-- >>> parseDeployment "http://localhost"
-- Right ("localhost",Nothing,False)
-- >>> parseDeployment "https://localhost"
-- Right ("localhost",Nothing,True)
-- >>> parseDeployment "http://localhost:8000"
-- Right ("localhost",Just 8000,False)
-- >>> parseDeployment "https://example.com:9443"
-- Right ("example.com",Just 9443,True)
-- >>> parseDeployment "http://example.localhost:8000"
-- Right ("example.localhost",Just 8000,False)
-- >>> parseDeployment "https://deployment.example.com:9443"
-- Right ("deployment.example.com",Just 9443,True)
parseDeployment :: T.Text -> Either String (T.Text, Maybe Int, Bool)
parseDeployment deployment = do
  uri <- maybeToEither "Can not parse the deployment URL" $ (U.parseAbsoluteURI . T.unpack) deployment
  isSecure <- maybeToEither "Unknown protocol encountered while parsing the deployment URL" $ (isSecureHttp . U.uriScheme) uri
  (shost, sport) <- maybeToEither "Unknown authority" $ (,) <$> U.uriRegName <*> U.uriPort <$> U.uriAuthority uri
  mport <- sequence $ maybeToEither "Can not parse the port information" . readMaybe . dropLeading ':' <$> nonEmptyString sport
  pure (T.pack shost, mport, isSecure)


-- | Checks if the protocol is secure HTTP or not.
--
-- If the protocol is not HTTP at all, returns @Nothing@.
--
-- >>> isSecureHttp "http"
-- Just False
-- >>> isSecureHttp "https"
-- Just True
-- >>> fmap isSecureHttp ["http", "https", "Http", "HTTP", "Https", "Https", "Http:", "Https:"]
-- [Just False,Just True,Just False,Just False,Just True,Just True,Just False,Just True]
-- >>> fmap isSecureHttp ["htt", "htts", "Htp", "HTP", "Htps", "Htps"]
-- [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
isSecureHttp :: String -> Maybe Bool
isSecureHttp = isSecure' . fmap C.toLower . dropTrailing ':'
  where
    isSecure' "http"  = Just False
    isSecure' "https" = Just True
    isSecure' _       = Nothing

------------------
-- END INTERNAL --
------------------
