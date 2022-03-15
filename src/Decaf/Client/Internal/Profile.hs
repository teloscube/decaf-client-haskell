-- | This module provides definitions to work on profiles.

{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE DerivingVia #-}

module Decaf.Client.Internal.Profile where

import           Control.Exception                 (IOException, catch)
import           Control.Monad.Catch               (MonadCatch, MonadThrow)
import           Control.Monad.IO.Class            (MonadIO(liftIO))
import qualified Data.ByteString                   as B
import           Data.Foldable                     (find)
import qualified Data.Text                         as T
import qualified Data.Yaml                         as Yaml
import           Decaf.Client.Internal.Credentials (Credentials)
import           Decaf.Client.Internal.Exception   (throwIOException, throwParseException)
import           Decaf.Client.Internal.Remote      (Remote)
import qualified Deriving.Aeson.Stock              as DAS
import           GHC.Stack                         (HasCallStack)


-- | A DECAF Instance user profile for constructing a 'DecafClient'.
--
-- >>> import Decaf.Client.Internal.Credentials
-- >>> import Decaf.Client.Internal.Remote
--
-- >>> Right remote = parseRemote "https://telostest.decafhub.com"
-- >>> let credentials = CredentialsBasic (BasicCredentials "username" "password")
--
-- >>> let json = Data.Aeson.encode (Profile "test" remote credentials)
-- >>> json
-- "{\"name\":\"test\",\"url\":\"https://telostest.decafhub.com\",\"credentials\":{\"type\":\"basic\",\"value\":{\"username\":\"username\",\"password\":\"password\"}}}"
-- >>> Data.Aeson.decode @Profile json
-- Just (Profile {profileName = "test", profileUrl = [https]://[telostest.decafhub.com]:[443], profileCredentials = <********>})
data Profile = Profile
  { profileName        :: !T.Text
  , profileUrl         :: !Remote
  , profileCredentials :: !Credentials
  }
  deriving (Eq, DAS.Generic, Show)
  deriving (DAS.FromJSON, DAS.ToJSON) via DAS.PrefixedSnake "profile" Profile


-- | Attempts to read and return profiles from a file at the given filepath.
readProfiles
  :: HasCallStack
  => MonadIO m
  => MonadCatch m
  => MonadThrow m
  => FilePath
  -> m [Profile]
readProfiles fp = liftIO (B.readFile fp `catch` transformIOException) >>= parseProfiles
  where
    transformIOException :: MonadThrow m => IOException -> m a
    transformIOException exc = throwIOException ("Cannot read profiles file: " <> T.pack fp) exc


-- | Attempts to parse and return profiles from given JSON contents.
parseProfiles
  :: HasCallStack
  => MonadIO m
  => MonadThrow m
  => B.ByteString
  -> m [Profile]
parseProfiles content = case Yaml.decodeEither' content of
  Left err -> throwParseException "Error while parsing profiles" (T.pack . show $ err)
  Right sp -> pure sp


-- | Attempts to find and return the profile by the given name in the given
-- profiles file.
readProfile
  :: HasCallStack
  => MonadIO m
  => MonadCatch m
  => MonadThrow m
  => T.Text
  -> FilePath
  -> m (Maybe Profile)
readProfile name fp = do
  profiles <- readProfiles fp
  pure $ find ((==) name . profileName) profiles
