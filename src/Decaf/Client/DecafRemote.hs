-- | This module provides auxiliaries to parse 'Remote's.

module Decaf.Client.DecafRemote where

import           Control.Monad.Except        (MonadError)
import qualified Data.Aeson                  as Aeson
import qualified Data.Char                   as C
import           Data.Maybe                  (fromMaybe)
import qualified Data.Text                   as T
import           Decaf.Client.Internal.Error (DecafClientError(unDecafClientError), throwDecafClientError)
import           Decaf.Client.Internal.Utils (dropLeading, dropTrailing, nonEmptyString)
import qualified Network.URI                 as U
import           Text.Printf                 (printf)
import           Text.Read                   (readMaybe)


-- | Type definition for addressing a remote DECAF Instance.
--
-- >>> DecafRemote "example.com" Nothing False
-- [http]://[example.com]:[80]
-- >>> DecafRemote "example.com" Nothing True
-- [https]://[example.com]:[443]
-- >>> DecafRemote "example.com" (Just 8080) False
-- [http]://[example.com]:[8080]
-- >>> DecafRemote "example.com" (Just 8443) True
-- [https]://[example.com]:[8443]
data DecafRemote = DecafRemote
  { decafRemoteHost   :: !T.Text
  , decafRemotePort   :: !(Maybe Int)
  , decafRemoteSecure :: !Bool
  }
  deriving Eq


instance Show DecafRemote where
  show (DecafRemote h p s) = printf "[%s]://[%s]:[%d]" s' h' p'
    where
      s' = (if s then "https" else "http") :: String
      h' = T.unpack h
      p' = fromMaybe (if s then 443 else 80) p


instance Aeson.FromJSON DecafRemote where
  parseJSON = Aeson.withText "Remote" $ \x -> case parseRemote x of
    Left err -> fail (unDecafClientError err)
    Right sr -> pure sr


instance Aeson.ToJSON DecafRemote where
  toJSON = Aeson.String . remoteToUrl


-- | Converts the 'DecafRemote' to a sanitized url.
--
-- >>> remoteToUrl (DecafRemote "localhost" (Just 8000) False)
-- "http://localhost:8000"
-- >>> remoteToUrl (DecafRemote "localhost" (Just 9443) True)
-- "https://localhost:9443"
-- >>> remoteToUrl (DecafRemote "localhost" (Just 80) False)
-- "http://localhost"
-- >>> remoteToUrl (DecafRemote "localhost" (Just 443) False)
-- "http://localhost:443"
-- >>> remoteToUrl (DecafRemote "localhost" (Just 80) True)
-- "https://localhost:80"
-- >>> remoteToUrl (DecafRemote "localhost" (Just 443) True)
-- "https://localhost"
-- >>> remoteToUrl (DecafRemote "localhost" Nothing False)
-- "http://localhost"
-- >>> remoteToUrl (DecafRemote "localhost" Nothing True)
-- "https://localhost"
remoteToUrl :: DecafRemote -> T.Text
remoteToUrl (DecafRemote h (Just 80) False) = "http://" <> h
remoteToUrl (DecafRemote h (Just 443) True) = "https://" <> h
remoteToUrl (DecafRemote h Nothing False)   = "http://" <> h
remoteToUrl (DecafRemote h Nothing True)    = "https://" <> h
remoteToUrl (DecafRemote h (Just p) False)  = "http://" <> h <> ":" <> T.pack (show p)
remoteToUrl (DecafRemote h (Just p) True)   = "https://" <> h <> ":" <> T.pack (show p)


-- | Attempts to parse a given URL as a DECAF Instance 'DecafRemote'.
--
-- >>> import Decaf.Client.Internal.Error
-- >>> import Decaf.Client.DecafRemote
-- >>> parseRemote "http://localhost" :: Either DecafClientError DecafRemote
-- Right [http]://[localhost]:[80]
-- >>> parseRemote "https://localhost" :: Either DecafClientError DecafRemote
-- Right [https]://[localhost]:[443]
-- >>> parseRemote "http://localhost:8080" :: Either DecafClientError DecafRemote
-- Right [http]://[localhost]:[8080]
-- >>> parseRemote "https://localhost:8443" :: Either DecafClientError DecafRemote
-- Right [https]://[localhost]:[8443]
-- >>> parseRemote "https://localhost:8443/path-segment-to-be-truncated" :: Either DecafClientError DecafRemote
-- Right [https]://[localhost]:[8443]
-- >>> parseRemote "" :: Either DecafClientError DecafRemote
-- Left (DecafClientError {unDecafClientError = "Can not parse remote url: ''"})
-- >>> parseRemote "weird" :: Either DecafClientError DecafRemote
-- Left (DecafClientError {unDecafClientError = "Can not parse remote url: 'weird'"})
-- >>> parseRemote "httpk://localhost:8443" :: Either DecafClientError DecafRemote
-- Left (DecafClientError {unDecafClientError = "Unknown protocol: httpk"})
-- >>> parseRemote "http:" :: Either DecafClientError DecafRemote
-- Left (DecafClientError {unDecafClientError = "Can not parse authority from URI: 'http:'"})
-- >>> parseRemote "http:/" :: Either DecafClientError DecafRemote
-- Left (DecafClientError {unDecafClientError = "Can not parse authority from URI: 'http:/'"})
-- >>> parseRemote "http://" :: Either DecafClientError DecafRemote
-- Left (DecafClientError {unDecafClientError = "Empty host value"})
-- >>> parseRemote "http://a:" :: Either DecafClientError DecafRemote
-- Left (DecafClientError {unDecafClientError = "Can not parse port from URI: 'URIAuth {uriUserInfo = \"\", uriRegName = \"a\", uriPort = \":\"}'"})
parseRemote :: MonadError DecafClientError m => T.Text -> m DecafRemote
parseRemote url = do
  uri <- parseUri' $ T.unpack url
  (h, p) <- parseHostPort' uri
  s <- parseIsSecure' uri
  pure $ DecafRemote h p s


-- | Parses the remote URI.
--
-- >>> import Decaf.Client.Internal.Error
-- >>> parseUri' "" :: Either DecafClientError U.URI
-- Left (DecafClientError {unDecafClientError = "Can not parse remote url: ''"})
-- >>> parseUri' "/" :: Either DecafClientError U.URI
-- Left (DecafClientError {unDecafClientError = "Can not parse remote url: '/'"})
-- >>> parseUri' "http://localhost" :: Either DecafClientError U.URI
-- Right http://localhost
-- >>> parseUri' "https://localhost:8443" :: Either DecafClientError U.URI
-- Right https://localhost:8443
parseUri' :: MonadError DecafClientError m => String -> m U.URI
parseUri' x = maybe err pure $ U.parseAbsoluteURI x
  where
    err = throwDecafClientError $ "Can not parse remote url: '" <> (x <> "'")


-- | Attempts to find out if the 'U.URI' scheme is secure HTTP or not.
parseIsSecure' :: MonadError DecafClientError m => U.URI -> m Bool
parseIsSecure' = isSecureHttp' . U.uriScheme


-- | Attempts to get the host and port values from the given 'U.URI'.
parseHostPort' :: MonadError DecafClientError m => U.URI -> m (T.Text, Maybe Int)
parseHostPort' uri = (\auth -> (,) <$> parseHost' auth <*> parsePort' auth) =<< parseAuthority' uri


-- | Attempts to extrat the URI authority ('U.URIAuth') from the given 'U.URI'.
parseAuthority' :: MonadError DecafClientError m => U.URI -> m U.URIAuth
parseAuthority' uri = maybe err pure $ U.uriAuthority uri
  where
    err = throwDecafClientError $ "Can not parse authority from URI: '" <> (show uri <> "'")


-- | Attempts to get a non-empty 'T.Text' value as the host from the given
-- 'U.URIAuth'.
parseHost' :: MonadError DecafClientError m => U.URIAuth -> m T.Text
parseHost' a = maybe err pure $ T.pack <$> (nonEmptyString . U.uriRegName) a
  where
    err = throwDecafClientError "Empty host value"


-- | Attempts to get the port from the given 'U.URIAuth'.
parsePort' :: MonadError DecafClientError m => U.URIAuth -> m (Maybe Int)
parsePort' uri = maybe err pure . sequence$ (readMaybe . dropLeading ':' <$> (nonEmptyString . U.uriPort) uri)
  where
    err = throwDecafClientError $ "Can not parse port from URI: '" <> (show uri <> "'")


-- | Predicate to define if the URI indicates secure HTTP or not.
--
-- >>> import Decaf.Client.Internal.Error
-- >>> isSecureHttp' "http" :: Either DecafClientError Bool
-- Right False
-- >>> isSecureHttp' "https" :: Either DecafClientError Bool
-- Right True
-- >>> fmap isSecureHttp' ["http", "https", "Http", "HTTP", "Https", "Https", "Http:", "Https:"] :: [Either DecafClientError Bool]
-- [Right False,Right True,Right False,Right False,Right True,Right True,Right False,Right True]
-- >>> fmap isSecureHttp' ["htt", "htts", "Htp", "HTP", "Htps", "Htps"] :: [Either DecafClientError Bool]
-- [Left (DecafClientError {unDecafClientError = "Unknown protocol: htt"}),Left (DecafClientError {unDecafClientError = "Unknown protocol: htts"}),Left (DecafClientError {unDecafClientError = "Unknown protocol: htp"}),Left (DecafClientError {unDecafClientError = "Unknown protocol: htp"}),Left (DecafClientError {unDecafClientError = "Unknown protocol: htps"}),Left (DecafClientError {unDecafClientError = "Unknown protocol: htps"})]
-- >>> fmap isSecureHttp' ["htt", "htts"] :: [Either DecafClientError Bool]
-- [Left (DecafClientError {unDecafClientError = "Unknown protocol: htt"}),Left (DecafClientError {unDecafClientError = "Unknown protocol: htts"})]
isSecureHttp' :: MonadError DecafClientError m => String -> m Bool
isSecureHttp' = isSecure' . fmap C.toLower . dropTrailing ':'
  where
    isSecure' "http"  = pure False
    isSecure' "https" = pure True
    isSecure' proto   = throwDecafClientError $ "Unknown protocol: " <> proto
