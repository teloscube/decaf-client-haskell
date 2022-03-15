-- | This module provides types and functions to transcode DECAF API requests.

{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE DerivingVia #-}

module Decaf.Client.Internal.Types where

import           Control.Monad.Except        (MonadError, throwError)
import qualified Data.ByteString.Lazy        as BL
import           Data.Maybe                  (fromMaybe)
import qualified Data.Text                   as T
import           Decaf.Client.Internal.Utils (dropTrailing)
import qualified Deriving.Aeson              as DA
import qualified Deriving.Aeson.Stock        as DAS
import           Text.Printf                 (printf)


-- | Type definition for DECAF client errors.
newtype DecafClientError = DecafClientError String deriving (Show)


-- | Throws a DECAF client error.
--
-- >>> throwDecafClientError "Invalid request" :: Either DecafClientError ()
-- Left (DecafClientError "Invalid request")
throwDecafClientError :: MonadError DecafClientError m => String -> m a
throwDecafClientError = throwError . DecafClientError


-- | Type definition for addressing a remote DECAF Instance.
--
-- >>> Remote "example.com" Nothing False
-- [http]://[example.com]:[80]
-- >>> Remote "example.com" Nothing True
-- [https]://[example.com]:[443]
-- >>> Remote "example.com" (Just 8080) False
-- [http]://[example.com]:[8080]
-- >>> Remote "example.com" (Just 8443) True
-- [https]://[example.com]:[8443]
data Remote = Remote
  { remoteHost   :: !T.Text
  , remotePort   :: !(Maybe Int)
  , remoteSecure :: !Bool
  }


instance Show Remote where
  show (Remote h p s) = printf "[%s]://[%s]:[%d]" s' h' p'
    where
      s' = (if s then "https" else "http") :: String
      h' = T.unpack h
      p' = fromMaybe (if s then 443 else 80) p


-- | Converts the 'Remote' to a sanitized url.
--
-- >>> remoteToUrl (Remote "localhost" (Just 8000) False)
-- "http://localhost:8000"
-- >>> remoteToUrl (Remote "localhost" (Just 9443) True)
-- "https://localhost:9443"
-- >>> remoteToUrl (Remote "localhost" (Just 80) False)
-- "http://localhost"
-- >>> remoteToUrl (Remote "localhost" (Just 443) False)
-- "http://localhost:443"
-- >>> remoteToUrl (Remote "localhost" (Just 80) True)
-- "https://localhost:80"
-- >>> remoteToUrl (Remote "localhost" (Just 443) True)
-- "https://localhost"
-- >>> remoteToUrl (Remote "localhost" Nothing False)
-- "http://localhost"
-- >>> remoteToUrl (Remote "localhost" Nothing True)
-- "https://localhost"
remoteToUrl :: Remote -> T.Text
remoteToUrl (Remote h (Just 80) False) = "http://" <> h
remoteToUrl (Remote h (Just 443) True) = "https://" <> h
remoteToUrl (Remote h Nothing False)   = "http://" <> h
remoteToUrl (Remote h Nothing True)    = "https://" <> h
remoteToUrl (Remote h (Just p) False)  = "http://" <> h <> ":" <> T.pack (show p)
remoteToUrl (Remote h (Just p) True)   = "https://" <> h <> ":" <> T.pack (show p)


-- | Type definition for high-level encoding of DECAF client requests.
data Request = Request
  { requestRemote            :: !Remote
  , requestNamespace         :: !Path
  , requestCredentials       :: !Credentials
  , requestUserAgent         :: !T.Text
  , requestHttpHeaders       :: !Headers
  , requestHttpMethod        :: !Method
  , requestHttpPath          :: !Path
  , requestHttpTrailingSlash :: !Bool
  , requestHttpParams        :: ![Param]
  , requestHttpPayload       :: !(Maybe Payload)
  }

instance Show Request where
  show x = dropTrailing '\n' $ unlines
    [ "Request {"
    , "  requestRemote            = " <> show (requestRemote x)
    , "  requestNamespace         = " <> show (requestNamespace x)
    , "  requestCredentials       = " <> show (requestCredentials x)
    , "  requestUserAgent         = " <> show (requestUserAgent x)
    , "  requestHttpHeaders       = " <> show (requestHttpHeaders x)
    , "  requestHttpMethod        = " <> show (requestHttpMethod x)
    , "  requestHttpPath          = " <> show (requestHttpPath x)
    , "  requestHttpTrailingSlash = " <> show (requestHttpTrailingSlash x)
    , "  requestHttpParams        = " <> show (requestHttpParams x)
    , "  requestHttpPayload       = " <> show (requestHttpPayload x)
    , "}"
    ]


-- | Data definition for DECAF API response values.
data Response a = Response
  { responseStatus  :: !Int
  , responseHeaders :: Headers
  , responseValue   :: !a
  } deriving Show


-- | Type definition for DECAF client request header key/value pair.
type Header = (T.Text, T.Text)


-- | Type definition for a list of DECAF client request 'Header's.
type Headers = [Header]


-- | Type definition for DECAF client request querystring parameter.
type Param = (T.Text, T.Text)


-- | Type definition for a list of DECAF client request 'Param's.
type Params = [Param]


-- | Type definition for a list of DECAF client request HTTP path segments.
newtype Path = MkPath { unPath :: [T.Text] } deriving Show


-- >>> mkPath "/a/b" <> mkPath "/c/d"
-- MkPath {unPath = ["a","b","c","d"]}
instance Semigroup Path where
  (<>) (MkPath p1) (MkPath p2) = MkPath (p1 <> p2)


-- >>> mconcat [mkPath "/a/b", mkPath "/c/d"]
-- MkPath {unPath = ["a","b","c","d"]}
instance Monoid Path where
  mempty = MkPath []
  mappend = (<>)
  mconcat ps = MkPath $ concatMap unPath ps


-- | Sanitizes the given 'T.Text' into path segments and builds a 'Path' value.
--
-- >>> mkPath ""
-- MkPath {unPath = []}
-- >>> mkPath "/"
-- MkPath {unPath = []}
-- >>> mkPath "//"
-- MkPath {unPath = []}
-- >>> mkPath "/a"
-- MkPath {unPath = ["a"]}
-- >>> mkPath "a/"
-- MkPath {unPath = ["a"]}
-- >>> mkPath "/a/"
-- MkPath {unPath = ["a"]}
-- >>> mkPath "/a/b"
-- MkPath {unPath = ["a","b"]}
-- >>> mkPath "a/b/"
-- MkPath {unPath = ["a","b"]}
-- >>> mkPath "/a/b/"
-- MkPath {unPath = ["a","b"]}
mkPath :: T.Text -> Path
mkPath = MkPath . filter ("" /=) . T.split ('/' ==)


-- | Data definition for available DECAF credentials types.
--
-- >>> Data.Aeson.encode (CredentialsHeader "some-header-value")
-- "{\"type\":\"Header\",\"value\":\"some-header-value\"}"
-- >>> Data.Aeson.encode (CredentialsBasic (BasicCredentials "some-username" "some-password"))
-- "{\"type\":\"Basic\",\"value\":{\"username\":\"some-username\",\"password\":\"some-password\"}}"
-- >>> Data.Aeson.encode (CredentialsKey (KeyCredentials "some-api-key" "some-api-secret"))
-- "{\"type\":\"Key\",\"value\":{\"key\":\"some-api-key\",\"secret\":\"some-api-secret\"}}"
-- >>> Data.Aeson.encode (CredentialsToken "some-api-token")
-- "{\"type\":\"Token\",\"value\":\"some-api-token\"}"
data Credentials =
    CredentialsHeader !T.Text
  | CredentialsBasic !BasicCredentials
  | CredentialsKey !KeyCredentials
  | CredentialsToken !T.Text
  deriving (Eq, DA.Generic)
  deriving (DA.FromJSON, DA.ToJSON) via DA.CustomJSON '[DA.ConstructorTagModifier (DA.StripPrefix "Credentials"), DA.SumTaggedObject "type" "value"] Credentials


instance Show Credentials where
  show _ = "<********>"


-- | Data definition for HTTP basic authentication credentials.
--
-- >>> let x = BasicCredentials "some-username" "some-password"
-- >>> Data.Aeson.encode x
-- "{\"username\":\"some-username\",\"password\":\"some-password\"}"
-- >>> Just x == Data.Aeson.decode @BasicCredentials (Data.Aeson.encode x)
-- True
data BasicCredentials = BasicCredentials
  { basicCredentialsUsername :: !T.Text
  , basicCredentialsPassword :: !T.Text
  }
  deriving (Eq, DA.Generic)
  deriving (DA.FromJSON, DA.ToJSON) via DAS.PrefixedSnake "basicCredentials" BasicCredentials


-- | Data definition for HTTP basic authentication credentials.
--
-- >>> let x = KeyCredentials "some-api-key" "some-api-secret"
-- >>> Data.Aeson.encode x
-- "{\"key\":\"some-api-key\",\"secret\":\"some-api-secret\"}"
-- >>> Just x == Data.Aeson.decode @KeyCredentials (Data.Aeson.encode x)
-- True
data KeyCredentials = KeyCredentials
  { keyCredentialsKey    :: !T.Text
  , keyCredentialsSecret :: !T.Text
  }
  deriving (Eq, DA.Generic)
  deriving (DA.FromJSON, DA.ToJSON) via DAS.PrefixedSnake "keyCredentials" KeyCredentials


-- | Available DECAF endpoint methods.
data Method = GET | POST | PUT | DELETE deriving (Show)


-- | Data definition for DECAF client request payloads.
data Payload = Payload
  { payloadType    :: !T.Text         -- ^ HTTP content type.
  , payloadContent :: !BL.ByteString  -- ^ HTTP payload body.
  }


instance Show Payload where
  show _ = "<TRUNCATED>"
