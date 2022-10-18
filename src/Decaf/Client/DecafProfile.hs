{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides definitions to work on profiles.
module Decaf.Client.DecafProfile where

import Control.Exception (IOException, catch)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import Data.Foldable (find)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Decaf.Client.DecafClientException (throwIOException, throwParseException)
import Decaf.Client.DecafCredentials (DecafCredentials)
import Decaf.Client.DecafRemote (DecafRemote)
import Decaf.Client.Internal.Utils (commonAesonOptions)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)


-- | A DECAF Instance user profile for constructing a
-- 'Decaf.Client.DecafClient.DecafClient'.
--
-- >>> import Decaf.Client.DecafCredentials
-- >>> import Decaf.Client.DecafRemote
--
-- >>> Right remote = parseRemote "https://telostest.decafhub.com"
-- >>> let credentials = DecafCredentialsBasic (DecafBasicCredentials "username" "password")
--
-- >>> let json = Data.Aeson.encode (DecafProfile "test" remote credentials)
-- >>> json
-- "{\"credentials\":{\"type\":\"basic\",\"value\":{\"password\":\"password\",\"username\":\"username\"}},\"name\":\"test\",\"remote\":\"https://telostest.decafhub.com\"}"
-- >>> Data.Aeson.decode @DecafProfile json
-- Just (DecafProfile {decafProfileName = "test", decafProfileRemote = https://telostest.decafhub.com, decafProfileCredentials = <********>})
data DecafProfile = DecafProfile
  { decafProfileName :: !T.Text
  , decafProfileRemote :: !DecafRemote
  , decafProfileCredentials :: !DecafCredentials
  }
  deriving (Eq, Generic, Show)


instance Aeson.FromJSON DecafProfile where
  parseJSON = Aeson.genericParseJSON $ commonAesonOptions "decafProfile"


instance Aeson.ToJSON DecafProfile where
  toJSON = Aeson.genericToJSON $ commonAesonOptions "decafProfile"


-- | Attempts to read and return profiles from a file at the given filepath.
readDecafProfiles
  :: HasCallStack
  => MonadIO m
  => MonadCatch m
  => MonadThrow m
  => FilePath
  -> m [DecafProfile]
readDecafProfiles fp = liftIO (B.readFile fp `catch` transformIOException) >>= parseDecafProfiles
  where
    transformIOException (x :: IOException) = throwIOException ("Cannot read profiles file: " <> T.pack fp) x


-- | Attempts to parse and return profiles from given JSON contents.
parseDecafProfiles
  :: HasCallStack
  => MonadIO m
  => MonadThrow m
  => B.ByteString
  -> m [DecafProfile]
parseDecafProfiles content = case Yaml.decodeEither' content of
  Left err -> throwParseException "Error while parsing profiles" (T.pack . show $ err)
  Right sp -> pure sp


-- | Attempts to find and return the profile by the given name in the given
-- profiles file.
readDecafProfile
  :: HasCallStack
  => MonadIO m
  => MonadCatch m
  => MonadThrow m
  => T.Text
  -> FilePath
  -> m (Maybe DecafProfile)
readDecafProfile name fp = do
  profiles <- readDecafProfiles fp
  pure $ find ((==) name . decafProfileName) profiles
