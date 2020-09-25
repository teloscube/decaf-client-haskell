-- | This module provides auxiliaries to build and work with 'Request' values.
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Decaf.Client.Internal.Request where

import qualified Data.Char                         as C
import qualified Data.Text                         as T
import           Decaf.Client.Internal.Combinators (credentials, header, host, port, secure)
import           Decaf.Client.Internal.Types       (Credentials(HeaderCredentials), Method(GET), Request(..))
import           Decaf.Client.Internal.Utils       (dropLeading, dropTrailing, nonEmptyString)
import           Decaf.Client.Version              (version)
import qualified Network.URI                       as U
import           Text.Printf                       (printf)
import           Text.Read                         (readMaybe)
import Control.Monad.Except (throwError, MonadError)


-- | Type definition for request errors.
newtype RequestError = RequestError String deriving (Show)


-- | Type definition for request monad error.
type RequestErrorM m = (MonadError RequestError m)


-- | Throws a request error.
--
-- >>> throwRequestError "Invalid request" :: Either RequestError ()
-- Left (RequestError "Invalid request")
throwRequestError :: RequestErrorM m => String -> m a
throwRequestError = throwError . RequestError


-- | Initializes a request with deployment URL and authentication credentials.
--
-- >>> initRequest "http://example.com" (HeaderCredentials "OUCH") :: Either RequestError Request
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
initRequest :: RequestErrorM m => T.Text -> Credentials -> m Request
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
-- >>> parseDeployment "http://localhost" :: Either RequestError (T.Text, Maybe Int, Bool)
-- Right ("localhost",Nothing,False)
-- >>> parseDeployment "https://localhost" :: Either RequestError (T.Text, Maybe Int, Bool)
-- Right ("localhost",Nothing,True)
-- >>> parseDeployment "http://localhost:8000" :: Either RequestError (T.Text, Maybe Int, Bool)
-- Right ("localhost",Just 8000,False)
-- >>> parseDeployment "https://example.com:9443" :: Either RequestError (T.Text, Maybe Int, Bool)
-- Right ("example.com",Just 9443,True)
-- >>> parseDeployment "http://example.localhost:8000" :: Either RequestError (T.Text, Maybe Int, Bool)
-- Right ("example.localhost",Just 8000,False)
-- >>> parseDeployment "https://deployment.example.com:9443" :: Either RequestError (T.Text, Maybe Int, Bool)
-- Right ("deployment.example.com",Just 9443,True)
parseDeployment :: RequestErrorM m => T.Text -> m (T.Text, Maybe Int, Bool)
parseDeployment deployment = parseHostProto =<< parseUri deployment


parseUri :: RequestErrorM m => T.Text -> m U.URI
parseUri url = case U.parseAbsoluteURI sUrl of
  Nothing  -> throwRequestError $ "Can not parse URL: " ++ sUrl
  Just uri -> pure uri
  where
    sUrl = T.unpack url


parseHostProto :: RequestErrorM m => U.URI -> m (T.Text, Maybe Int, Bool)
parseHostProto uri = do
  authority <- parseAuthority' uri
  let h = U.uriRegName authority
  p <- parsePort' authority
  s <- isSecureHttp $ U.uriScheme uri
  pure (T.pack h, p, s)
  where
    parseAuthority' :: RequestErrorM m1 => U.URI -> m1 U.URIAuth
    parseAuthority' x = maybe (throwRequestError "Can not parse authority") pure $ U.uriAuthority x

    parsePort' :: RequestErrorM m1 => U.URIAuth -> m1 (Maybe Int)
    parsePort' x = maybe (throwRequestError "Can not parse port") pure (sequence (readMaybe . dropLeading ':' <$> (nonEmptyString . U.uriPort) x))


-- | Checks if the protocol is secure HTTP or not.
--
-- If the protocol is not HTTP at all, returns @Nothing@.
--
-- >>> isSecureHttp "http" :: Either RequestError Bool
-- Right False
-- >>> isSecureHttp "https" :: Either RequestError Bool
-- Right True
-- >>> fmap isSecureHttp ["http", "https", "Http", "HTTP", "Https", "Https", "Http:", "Https:"] :: [Either RequestError Bool]
-- [Right False,Right True,Right False,Right False,Right True,Right True,Right False,Right True]
-- >>> fmap isSecureHttp ["htt", "htts", "Htp", "HTP", "Htps", "Htps"] :: [Either RequestError Bool]
-- [Left (RequestError "Unknown protocol: htt"),Left (RequestError "Unknown protocol: htts"),Left (RequestError "Unknown protocol: htp"),Left (RequestError "Unknown protocol: htp"),Left (RequestError "Unknown protocol: htps"),Left (RequestError "Unknown protocol: htps")]
-- >>> fmap isSecureHttp ["htt", "htts"] :: [Either RequestError Bool]
-- [Left (RequestError "Unknown protocol: htt"),Left (RequestError "Unknown protocol: htts")]
isSecureHttp :: RequestErrorM m => String -> m Bool
isSecureHttp = isSecure' . fmap C.toLower . dropTrailing ':'
  where
    isSecure' "http"  = pure False
    isSecure' "https" = pure True
    isSecure' proto   = throwError $ RequestError $ "Unknown protocol: " ++ proto


------------------
-- END INTERNAL --
------------------
