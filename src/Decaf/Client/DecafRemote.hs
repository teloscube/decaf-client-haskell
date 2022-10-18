{-# LANGUAGE OverloadedStrings #-}

-- | This module provides definitions and functions to work with DECAF Instance
-- remotes.

module Decaf.Client.DecafRemote where

import qualified Data.Aeson                  as Aeson
import qualified Data.Char                   as C
import qualified Data.Text                   as T
import           Decaf.Client.Internal.Utils (dropLeading, dropTrailing, nonEmptyString)
import qualified Network.URI                 as U
import           Text.Read                   (readMaybe)


-- | Type definition for addressing a remote DECAF Instance.
--
-- >>> DecafRemote "example.com" Nothing False
-- http://example.com
-- >>> DecafRemote "example.com" Nothing True
-- https://example.com
-- >>> DecafRemote "example.com" (Just 8080) False
-- http://example.com:8080
-- >>> DecafRemote "example.com" (Just 8443) True
-- https://example.com:8443
data DecafRemote = DecafRemote
  { decafRemoteHost   :: !T.Text
  , decafRemotePort   :: !(Maybe Int)
  , decafRemoteSecure :: !Bool
  }
  deriving (Eq, Ord)


instance Show DecafRemote where
  show = T.unpack . remoteToUrl


instance Aeson.FromJSON DecafRemote where
  parseJSON = Aeson.withText "Remote" $ either (fail . show) pure . parseRemote


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
-- >>> parseRemote "http://localhost"
-- Right http://localhost
-- >>> parseRemote "https://localhost"
-- Right https://localhost
-- >>> parseRemote "http://localhost:8080"
-- Right http://localhost:8080
-- >>> parseRemote "https://localhost:8443"
-- Right https://localhost:8443
-- >>> parseRemote "https://localhost:8443/path-segment-to-be-truncated"
-- Right https://localhost:8443
-- >>> parseRemote ""
-- Left "Can not parse remote url: ''"
-- >>> parseRemote "weird"
-- Left "Can not parse remote url: 'weird'"
-- >>> parseRemote "httpk://localhost:8443"
-- Left "Unknown protocol: httpk"
-- >>> parseRemote "http:"
-- Left "Can not parse authority from URI: 'http:'"
-- >>> parseRemote "http:/"
-- Left "Can not parse authority from URI: 'http:/'"
-- >>> parseRemote "http://"
-- Left "Empty host value"
-- >>> parseRemote "http://a:"
-- Left "Can not parse port from URI: 'URIAuth {uriUserInfo = \"\", uriRegName = \"a\", uriPort = \":\"}'"
parseRemote :: T.Text -> Either T.Text DecafRemote
parseRemote url = do
  uri <- parseUri' (T.unpack url)
  (h, p) <- parseHostPort' uri
  s <- parseIsSecure' uri
  pure $ DecafRemote h p s


-- | Parses the remote URI.
--
-- >>> parseUri' ""
-- Left "Can not parse remote url: ''"
-- >>> parseUri' "/"
-- Left "Can not parse remote url: '/'"
-- >>> parseUri' "http://localhost"
-- Right http://localhost
-- >>> parseUri' "https://localhost:8443"
-- Right https://localhost:8443
parseUri' :: String -> Either T.Text U.URI
parseUri' x =
  maybe (Left (T.pack ("Can not parse remote url: '" <> x <> "'"))) pure (U.parseAbsoluteURI x)


-- | Attempts to find out if the 'U.URI' scheme is secure HTTP or not.
parseIsSecure' :: U.URI -> Either T.Text Bool
parseIsSecure' = isSecureHttp' . U.uriScheme


-- | Attempts to get the host and port values from the given 'U.URI'.
parseHostPort' :: U.URI -> Either T.Text (T.Text, Maybe Int)
parseHostPort' uri = (\auth -> (,) <$> parseHost' auth <*> parsePort' auth) =<< parseAuthority' uri


-- | Attempts to extrat the URI authority ('U.URIAuth') from the given 'U.URI'.
parseAuthority' :: U.URI -> Either T.Text U.URIAuth
parseAuthority' uri =
  maybe (Left (T.pack ("Can not parse authority from URI: '" <> show uri <> "'"))) pure (U.uriAuthority uri)


-- | Attempts to get a non-empty 'T.Text' value as the host from the given
-- 'U.URIAuth'.
parseHost' :: U.URIAuth -> Either T.Text T.Text
parseHost' = maybe (Left "Empty host value") (Right . T.pack) . nonEmptyString . U.uriRegName


-- | Attempts to get the port from the given 'U.URIAuth'.
parsePort' :: U.URIAuth -> Either T.Text (Maybe Int)
parsePort' uri = maybe err pure (mapM (readMaybe . dropLeading ':') ((nonEmptyString . U.uriPort) uri))
  where
    err = Left $ T.pack ("Can not parse port from URI: '" <> (show uri <> "'"))


-- | Predicate to define if the URI indicates secure HTTP or not.
--
-- >>> isSecureHttp' "http"
-- Right False
-- >>> isSecureHttp' "https"
-- Right True
-- >>> fmap isSecureHttp' ["http", "https", "Http", "HTTP", "Https", "Https", "Http:", "Https:"]
-- [Right False,Right True,Right False,Right False,Right True,Right True,Right False,Right True]
-- >>> fmap isSecureHttp' ["htt", "htts", "Htp", "HTP", "Htps", "Htps"]
-- [Left "Unknown protocol: htt",Left "Unknown protocol: htts",Left "Unknown protocol: htp",Left "Unknown protocol: htp",Left "Unknown protocol: htps",Left "Unknown protocol: htps"]
-- >>> fmap isSecureHttp' ["htt", "htts"]
-- [Left "Unknown protocol: htt",Left "Unknown protocol: htts"]
isSecureHttp' :: String -> Either T.Text Bool
isSecureHttp' x = case fmap C.toLower (dropTrailing ':' x) of
  "http"  -> Right False
  "https" -> Right True
  proto   -> Left (T.pack ("Unknown protocol: " <> proto))
