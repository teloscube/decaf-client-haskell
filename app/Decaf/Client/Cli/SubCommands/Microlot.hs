{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides the runner for batch DECAF Microlot query for
-- requested profiles.
module Decaf.Client.Cli.SubCommands.Microlot where

import Control.Exception (IOException, SomeException)
import Control.Monad.Catch (MonadCatch (catch), MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Control.Monad.Parallel as MP
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Decaf.Client as DC
import GHC.Stack (HasCallStack)


-- | Data definition for DECAF Microlot batch run configuration.
data MicrolotRunConfig = MicrolotRunConfig
  { microlotRunConfigFile :: !FilePath
  , microlotRunConfigProfileName :: !(Maybe T.Text)
  , microlotRunConfigQuery :: !FilePath
  , microlotRunConfigQueryParams :: !(Maybe Aeson.Value)
  }


-- | Attempts to run the config.
runMicrolot
  :: HasCallStack
  => MP.MonadParallel m
  => MonadCatch m
  => MonadThrow m
  => MonadIO m
  => MicrolotRunConfig
  -> m ()
runMicrolot MicrolotRunConfig {..} = do
  allProfiles <- DC.readDecafProfiles microlotRunConfigFile
  let profiles = case microlotRunConfigProfileName of
        Nothing -> allProfiles
        Just sn -> filter (\x -> DC.decafProfileName x == sn) allProfiles
  results <- MP.mapM (runQueryForProfile microlotRunConfigQuery microlotRunConfigQueryParams) profiles
  let zipped =
        Aeson.Array . V.fromList $
          (\(x, y) -> Aeson.object ["name" .= DC.decafProfileName x, "remote" .= DC.decafProfileRemote x, "result" .= y])
            <$> zip profiles results
  liftIO . BL.putStr $ Aeson.encode zipped


-- | Attempts to run the query for the profile.
runQueryForProfile
  :: HasCallStack
  => MonadCatch m
  => MonadThrow m
  => MonadIO m
  => FilePath
  -> Maybe Aeson.Value
  -> DC.DecafProfile
  -> m Aeson.Value
runQueryForProfile fp vars profile = do
  gql <- liftIO (readFile fp `catch` transformIOException)
  let client = DC.mkDecafClientFromProfile profile
  attempt gql client `catch` handle
  where
    attempt q = DC.runDecafMicrolot q vars
    handle (x :: SomeException) = pure (Aeson.String (T.pack . show $ x))
    transformIOException (x :: IOException) = DC.throwIOException (T.pack $ "Cannot read GraphQL query file: " <> fp) x
