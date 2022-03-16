-- | This module provides definitions to work on profiles.

{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE DerivingVia #-}

module Decaf.Client.DecafProfile where

import           Control.Exception                 (IOException, catch)
import           Control.Monad.Catch               (MonadCatch, MonadThrow)
import           Control.Monad.IO.Class            (MonadIO(liftIO))
import qualified Data.ByteString                   as B
import           Data.Foldable                     (find)
import qualified Data.Text                         as T
import qualified Data.Yaml                         as Yaml
import           Decaf.Client.DecafClientException (throwIOException, throwParseException)
import           Decaf.Client.DecafCredentials     (DecafCredentials)
import           Decaf.Client.DecafRemote          (DecafRemote)
import qualified Deriving.Aeson.Stock              as DAS
import           GHC.Stack                         (HasCallStack)


-- | A DECAF Instance user profile for constructing a 'DecafClient'.
--
-- >>> import Decaf.Client.DecafCredentials
-- >>> import Decaf.Client.DecafRemote
--
-- >>> Right remote = parseRemote "https://telostest.decafhub.com"
-- >>> let credentials = DecafCredentialsBasic (DecafBasicCredentials "username" "password")
--
-- >>> let json = Data.Aeson.encode (DecafProfile "test" remote credentials)
-- >>> json
-- "{\"name\":\"test\",\"remote\":\"https://telostest.decafhub.com\",\"credentials\":{\"type\":\"basic\",\"value\":{\"username\":\"username\",\"password\":\"password\"}}}"
-- >>> Data.Aeson.decode @DecafProfile json
-- Just (DecafProfile {decafProfileName = "test", decafProfileRemote = [https]://[telostest.decafhub.com]:[443], decafProfileCredentials = <********>})
data DecafProfile = DecafProfile
  { decafProfileName        :: !T.Text
  , decafProfileRemote      :: !DecafRemote
  , decafProfileCredentials :: !DecafCredentials
  }
  deriving (Eq, DAS.Generic, Show)
  deriving (DAS.FromJSON, DAS.ToJSON) via DAS.PrefixedSnake "decafProfile" DecafProfile


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
    transformIOException :: MonadThrow m => IOException -> m a
    transformIOException exc = throwIOException ("Cannot read profiles file: " <> T.pack fp) exc


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
