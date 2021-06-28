-- | This module provides auxiliaries to parse 'Remote's.
--
module Decaf.Client.Internal.Remote where

import qualified Data.Char                   as C
import           Data.Maybe                  (fromMaybe)
import qualified Data.Text                   as T
import           Decaf.Client.Internal.Types (DecafClientM, Remote(Remote), throwDecafClientError)
import           Decaf.Client.Internal.Utils (dropLeading, dropTrailing, nonEmptyString)
import qualified Network.URI                 as U
import           Text.Printf                 (printf)
import           Text.Read                   (readMaybe)


-- | Renders the 'Remote' value into a base DECAF deployment URL.
--
-- >>> remoteUrl $ Remote "localhost" Nothing False
-- "http://localhost:80"
-- >>> remoteUrl $ Remote "localhost" Nothing True
-- "https://localhost:443"
-- >>> remoteUrl $ Remote "localhost" (Just 8080) False
-- "http://localhost:8080"
-- >>> remoteUrl $ Remote "localhost" (Just 8443) True
-- "https://localhost:8443"
remoteUrl :: Remote -> T.Text
remoteUrl (Remote h p s) = T.pack $ printf "%s://%s:%d" s' h' p'
  where
    s' = (if s then "https" else "http") :: String
    h' = T.unpack h
    p' = fromMaybe (if s then 443 else 80) p


-- | Attempts to parse a given URL as a DECAF deployment 'Remote'.
--
-- >>> import Decaf.Client
-- >>> parseRemote "http://localhost" :: Either DecafClientError Remote
-- Right [http]://[localhost]:[80]
-- >>> parseRemote "https://localhost" :: Either DecafClientError Remote
-- Right [https]://[localhost]:[443]
-- >>> parseRemote "http://localhost:8080" :: Either DecafClientError Remote
-- Right [http]://[localhost]:[8080]
-- >>> parseRemote "https://localhost:8443" :: Either DecafClientError Remote
-- Right [https]://[localhost]:[8443]
-- >>> parseRemote "https://localhost:8443/path-segment-to-be-truncated" :: Either DecafClientError Remote
-- Right [https]://[localhost]:[8443]
-- >>> parseRemote "" :: Either DecafClientError Remote
-- Left (DecafClientError "Can not parse remote url: ''")
-- >>> parseRemote "weird" :: Either DecafClientError Remote
-- Left (DecafClientError "Can not parse remote url: 'weird'")
-- >>> parseRemote "httpk://localhost:8443" :: Either DecafClientError Remote
-- Left (DecafClientError "Unknown protocol: httpk")
-- >>> parseRemote "http:" :: Either DecafClientError Remote
-- Left (DecafClientError "Can not parse authority from URI: 'http:'")
-- >>> parseRemote "http:/" :: Either DecafClientError Remote
-- Left (DecafClientError "Can not parse authority from URI: 'http:/'")
-- >>> parseRemote "http://" :: Either DecafClientError Remote
-- Left (DecafClientError "Empty host value")
-- >>> parseRemote "http://a:" :: Either DecafClientError Remote
-- Left (DecafClientError "Can not parse port from URI: 'URIAuth {uriUserInfo = \"\", uriRegName = \"a\", uriPort = \":\"}'")
parseRemote :: DecafClientM m => T.Text -> m Remote
parseRemote url = do
  uri <- parseUri' $ T.unpack url
  (h, p) <- parseHostPort' uri
  s <- parseIsSecure' uri
  pure $ Remote h p s


-- | Parses the remote URI.
--
-- >>> import Decaf.Client
-- >>> parseUri' "" :: Either DecafClientError U.URI
-- Left (DecafClientError "Can not parse remote url: ''")
-- >>> parseUri' "/" :: Either DecafClientError U.URI
-- Left (DecafClientError "Can not parse remote url: '/'")
-- >>> parseUri' "http://localhost" :: Either DecafClientError U.URI
-- Right http://localhost
-- >>> parseUri' "https://localhost:8443" :: Either DecafClientError U.URI
-- Right https://localhost:8443
parseUri' :: DecafClientM m => String -> m U.URI
parseUri' x = maybe err pure $ U.parseAbsoluteURI x
  where
    err = throwDecafClientError $ "Can not parse remote url: '" ++ x ++ "'"


-- | Attempts to find out if the 'U.URI' scheme is secure HTTP or not.
parseIsSecure' :: DecafClientM m => U.URI -> m Bool
parseIsSecure' = isSecureHttp' . U.uriScheme


-- | Attempts to get the host and port values from the given 'U.URI'.
parseHostPort' :: DecafClientM m => U.URI -> m (T.Text, Maybe Int)
parseHostPort' uri = (\auth -> (,) <$> parseHost' auth <*> parsePort' auth) =<< parseAuthority' uri


-- | Attempts to extrat the URI authority ('U.URIAuth') from the given 'U.URI'.
parseAuthority' :: DecafClientM m => U.URI -> m U.URIAuth
parseAuthority' uri = maybe err pure $ U.uriAuthority uri
  where
    err = throwDecafClientError $ "Can not parse authority from URI: '" ++ show uri ++ "'"


-- | Attempts to get a non-empty 'T.Text' value as the host from the given 'U.URIAuth'.
parseHost' :: DecafClientM m => U.URIAuth -> m T.Text
parseHost' a = maybe err pure $ T.pack <$> (nonEmptyString . U.uriRegName) a
  where
    err = throwDecafClientError "Empty host value"


-- | Attempts to get the port from the given 'U.URIAuth'.
parsePort' :: DecafClientM m => U.URIAuth -> m (Maybe Int)
parsePort' uri = maybe err pure $ sequence $ readMaybe . dropLeading ':' <$> (nonEmptyString . U.uriPort) uri
  where
    err = throwDecafClientError $ "Can not parse port from URI: '" ++ show uri ++ "'"


-- | Predicate to define if the URI indicates secure HTTP or not.
--
-- >>> import Decaf.Client
-- >>> isSecureHttp' "http" :: Either DecafClientError Bool
-- Right False
-- >>> isSecureHttp' "https" :: Either DecafClientError Bool
-- Right True
-- >>> fmap isSecureHttp' ["http", "https", "Http", "HTTP", "Https", "Https", "Http:", "Https:"] :: [Either DecafClientError Bool]
-- [Right False,Right True,Right False,Right False,Right True,Right True,Right False,Right True]
-- >>> fmap isSecureHttp' ["htt", "htts", "Htp", "HTP", "Htps", "Htps"] :: [Either DecafClientError Bool]
-- [Left (DecafClientError "Unknown protocol: htt"),Left (DecafClientError "Unknown protocol: htts"),Left (DecafClientError "Unknown protocol: htp"),Left (DecafClientError "Unknown protocol: htp"),Left (DecafClientError "Unknown protocol: htps"),Left (DecafClientError "Unknown protocol: htps")]
-- >>> fmap isSecureHttp' ["htt", "htts"] :: [Either DecafClientError Bool]
-- [Left (DecafClientError "Unknown protocol: htt"),Left (DecafClientError "Unknown protocol: htts")]
isSecureHttp' :: DecafClientM m => String -> m Bool
isSecureHttp' = isSecure' . fmap C.toLower . dropTrailing ':'
  where
    isSecure' "http"  = pure False
    isSecure' "https" = pure True
    isSecure' proto   = throwDecafClientError $ "Unknown protocol: " ++ proto
