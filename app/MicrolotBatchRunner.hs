-- | This module provides the runner for batch DECAF Microlot query for
-- requested profiles.

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MicrolotBatchRunner where

import           Control.Exception      (IOException, SomeException)
import           Control.Monad.Catch    (MonadCatch(catch), MonadThrow)
import           Control.Monad.IO.Class (MonadIO(..))
import qualified Control.Monad.Parallel as MP
import           Data.Aeson             ((.=))
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as BC
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Text              as T
import qualified Data.Vector            as V
import           Decaf.Client
                 ( DecafGraphqlQuery
                 , DecafProfile(decafProfileName, decafProfileRemote)
                 , decafGraphqlQuery
                 , decafGraphqlQueryNoVars
                 , mkDecafClientFromProfile
                 , readDecafProfiles
                 , runDecafMicrolotRequestJson
                 , throwIOException
                 )
import           GHC.Stack              (HasCallStack)


-- | Data definition for DECAF Microlot batch run configuration.
data MicrolotBatchRunConfig = MicrolotBatchRunConfig
  { microlotBatchRunConfigFile        :: !FilePath
  , microlotBatchRunConfigProfileName :: !(Maybe T.Text)
  , microlotBatchRunConfigQuery       :: !FilePath
  , microlotBatchRunConfigQueryParams :: !(Maybe Aeson.Value)
  }


-- | Attempts to run the config.
runBatchMicrolot
  :: HasCallStack
  => MP.MonadParallel m
  => MonadCatch m
  => MonadThrow m
  => MonadIO m
  => MicrolotBatchRunConfig
  -> m ()
runBatchMicrolot MicrolotBatchRunConfig{..} = do
  allProfiles <- readDecafProfiles microlotBatchRunConfigFile
  let profiles = case microlotBatchRunConfigProfileName of
        Nothing -> allProfiles
        Just sn -> filter (\x -> decafProfileName x == sn) allProfiles
  results <- MP.mapM (runQueryForProfile microlotBatchRunConfigQuery microlotBatchRunConfigQueryParams) profiles
  let zipped = Aeson.Array . V.fromList
          $ (\(x, y) -> Aeson.object ["name" .= decafProfileName x, "remote" .= decafProfileRemote x, "result" .= y])
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
  -> DecafProfile
  -> m Aeson.Value
runQueryForProfile fp vars profile = do
  query <- buildMicrolotQuery fp vars
  let client = mkDecafClientFromProfile profile
  attempt query client `catch` handle
  where
    attempt q c = runDecafMicrolotRequestJson q c
    handle = \(x :: SomeException) -> pure (Aeson.String (T.pack . show $ x))


-- | Attempts to build a 'MicrolotQuery' from the given GQL file path and
-- optional query variables.
buildMicrolotQuery
  :: HasCallStack
  => MonadCatch m
  => MonadThrow m
  => MonadIO m
  => FilePath
  -> Maybe Aeson.Value
  -> m (DecafGraphqlQuery Aeson.Value)
buildMicrolotQuery fp mVars = do
  gql <- BC.unpack <$> liftIO (B.readFile fp `catch` transformIOException)
  case mVars of
    Nothing -> pure $ decafGraphqlQueryNoVars gql
    Just sv -> pure $ decafGraphqlQuery gql sv
  where
    transformIOException :: MonadThrow m => IOException -> m a
    transformIOException exc = throwIOException (T.pack $ "Cannot read GraphQL query file: " <> fp) exc
