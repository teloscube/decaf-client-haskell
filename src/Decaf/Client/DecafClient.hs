-- | This module provides high-level DECAF API client and related definitions.

{-# LANGUAGE RecordWildCards #-}

module Decaf.Client.DecafClient where

import           Control.Monad.Catch               (MonadCatch, MonadThrow)
import qualified Data.Text                         as T

import           Control.Monad.IO.Class            (MonadIO)
import qualified Data.Aeson                        as Aeson
import qualified Data.ByteString.Lazy              as BL
import qualified Data.Text.Lazy                    as TL
import qualified Data.Text.Lazy.Encoding           as TLE
import           Decaf.Client.DecafClientException (throwRemoteException, throwRequestException)
import           Decaf.Client.DecafCredentials     (DecafCredentials)
import           Decaf.Client.DecafProfile         (DecafProfile(..))
import           Decaf.Client.DecafRemote          (DecafRemote, parseRemote)
import           Decaf.Client.DecafRequest
                 ( DecafRequest(decafRequestRemote)
                 , DecafRequestCombinator
                 , apiBarista
                 , apiBeanbag
                 , apiEstate
                 , apiFunctions
                 , apiMicrolot
                 , apiModulePdms
                 , graphql
                 , graphqlNoVars
                 , initRequest
                 )
import           Decaf.Client.DecafResponse        (DecafGraphqlQueryResult(..), DecafResponse(decafResponseBody))
import           Decaf.Client.Internal.Http        (performDecafRequest, performDecafRequestJson)
import           GHC.Stack                         (HasCallStack)


-- * DECAF Client
-- $dataDefinition


-- | Data definition that represents a DECAF API client.
newtype DecafClient = DecafClient
  { unDecafClient :: DecafRequest
  }


-- | Returns the 'DecafRemote' for the 'DecafClient'.
decafClientRemote :: DecafClient -> DecafRemote
decafClientRemote = decafRequestRemote . unDecafClient


-- * Constructors
-- $constructors


-- | Builds a 'DecafClient' from a given 'DecafRemote' and 'DecafCredentials'.
mkDecafClient
  :: DecafRemote
  -> DecafCredentials
  -> DecafClient
mkDecafClient r c = DecafClient (initRequest r c)


-- | Attempts to builds a 'DecafClient' from a given DECAF Instance URL and
-- 'DecafCredentials'.
mkDecafClientM
  :: HasCallStack
  => MonadThrow m
  => T.Text
  -> DecafCredentials
  -> m DecafClient
mkDecafClientM u c = either throwRemoteException (pure . flip mkDecafClient c) (parseRemote u)


-- | Builds a 'DecafClient' from the given 'DecafProfile'.
mkDecafClientFromProfile
  :: DecafProfile
  -> DecafClient
mkDecafClientFromProfile DecafProfile{..} =
  mkDecafClient decafProfileRemote decafProfileCredentials


-- * Request Builders
-- $requestBuilders


-- | Builds a 'DecafRequest' for the given 'DecafClient' with the given
-- 'DecafRequestCombinator'.
buildDecafRequest :: DecafRequestCombinator -> DecafClient -> DecafRequest
buildDecafRequest c = c . unDecafClient


-- * Runners
-- $runners


-- ** Core Runners
-- $coreRunners


-- | Attempts to runs a given 'DecafRequestCombinator' over a given
-- 'DecafClient', pick the successful response body and return it as
-- 'BL.ByteString'.
runDecafClient
  :: HasCallStack
  => MonadCatch m
  => MonadThrow m
  => MonadIO m
  => DecafRequestCombinator
  -> DecafClient
  -> m BL.ByteString
runDecafClient c dc = decafResponseBody <$> performDecafRequest (buildDecafRequest c dc)


-- | Attempts to runs a given 'DecafRequestCombinator' over a given
-- 'DecafClient', pick the successful response body, decode it into the
-- requested type @a@ and return it.
runDecafClientJson
  :: HasCallStack
  => Aeson.FromJSON a
  => MonadCatch m
  => MonadThrow m
  => MonadIO m
  => DecafRequestCombinator
  -> DecafClient
  -> m a
runDecafClientJson c dc = decafResponseBody <$> performDecafRequestJson (buildDecafRequest c dc)


-- ** Barista Runners
-- $baristaRunners


-- | Convenience function for 'runDecafClient' that hits DECAF Barista API
-- namespace.
runDecafBarista
  :: HasCallStack
  => MonadCatch m
  => MonadThrow m
  => MonadIO m
  => DecafRequestCombinator
  -> DecafClient
  -> m BL.ByteString
runDecafBarista c = runDecafClient (c . apiBarista)


-- | Convenience function for 'runDecafClientJson' that hits DECAF Barista API
-- namespace.
runDecafBaristaJson
  :: HasCallStack
  => Aeson.FromJSON a
  => MonadCatch m
  => MonadThrow m
  => MonadIO m
  => DecafRequestCombinator
  -> DecafClient
  -> m a
runDecafBaristaJson c = runDecafClientJson (c . apiBarista)


-- ** Estate Runners
-- $estateRunners


-- | Convenience function for 'runDecafClient' that hits DECAF Estate API
-- namespace.
runDecafEstate
  :: HasCallStack
  => MonadCatch m
  => MonadThrow m
  => MonadIO m
  => DecafRequestCombinator
  -> DecafClient
  -> m BL.ByteString
runDecafEstate c = runDecafClient (c . apiEstate)


-- | Convenience function for 'runDecafClientJson' that hits DECAF Estate API
-- namespace.
runDecafEstateJson
  :: HasCallStack
  => Aeson.FromJSON a
  => MonadCatch m
  => MonadThrow m
  => MonadIO m
  => DecafRequestCombinator
  -> DecafClient
  -> m a
runDecafEstateJson c = runDecafClientJson (c . apiEstate)


-- ** Function Runners
-- $functionRunners


-- | Convenience function for 'runDecafClient' that hits DECAF Functions API
-- namespace.
runDecafFunction
  :: HasCallStack
  => MonadCatch m
  => MonadThrow m
  => MonadIO m
  => DecafRequestCombinator
  -> DecafClient
  -> m BL.ByteString
runDecafFunction c = runDecafClient (c . apiFunctions)


-- | Convenience function for 'runDecafClientJson' that hits DECAF Functions API
-- namespace.
runDecafFunctionJson
  :: HasCallStack
  => Aeson.FromJSON a
  => MonadCatch m
  => MonadThrow m
  => MonadIO m
  => DecafRequestCombinator
  -> DecafClient
  -> m a
runDecafFunctionJson c = runDecafClientJson (c . apiFunctions)


-- ** Beanbag Runners
-- $beanbagRunners


-- | Convenience function for 'runDecafClient' that hits DECAF Beanbag API
-- namespace.
runDecafBeanbag
  :: HasCallStack
  => MonadCatch m
  => MonadThrow m
  => MonadIO m
  => DecafRequestCombinator
  -> DecafClient
  -> m BL.ByteString
runDecafBeanbag c = runDecafClient (c . apiBeanbag)


-- | Convenience function for 'runDecafClientJson' that hits DECAF Beanbag API
-- namespace.
runDecafBeanbagJson
  :: HasCallStack
  => Aeson.FromJSON a
  => MonadCatch m
  => MonadThrow m
  => MonadIO m
  => DecafRequestCombinator
  -> DecafClient
  -> m a
runDecafBeanbagJson c = runDecafClientJson (c . apiBeanbag)


-- ** GraphQL Runners
-- $graphqlRunners


-- | Convenience function for 'runDecafClientJson' that performs GraphQL queries
-- as per given 'DecafRequestCombinator'.
runDecafGraphql
  :: HasCallStack
  => Aeson.FromJSON a
  => Aeson.ToJSON b
  => MonadCatch m
  => MonadThrow m
  => MonadIO m
  => DecafRequestCombinator
  -> String
  -> b
  -> DecafClient
  -> m a
runDecafGraphql c gql vars dc = do
  value <- runDecafClientJson (graphql gql vars . c) dc
  case value of
    DecafGraphqlQueryResultSuccess x -> pure x
    DecafGraphqlQueryResultFailure v -> throwRequestException ("Error during parsing GraphQL response: " <> TL.toStrict (TLE.decodeUtf8 (Aeson.encode v)))


-- | Convenience function for 'runDecafClientJson' that performs GraphQL queries
-- (without query variables) as per given 'DecafRequestCombinator'.
runDecafGraphqlNoVars
  :: HasCallStack
  => Aeson.FromJSON a
  => MonadCatch m
  => MonadThrow m
  => MonadIO m
  => DecafRequestCombinator
  -> String
  -> DecafClient
  -> m a
runDecafGraphqlNoVars c gql dc = do
  value <- runDecafClientJson (graphqlNoVars gql . c) dc
  case value of
    DecafGraphqlQueryResultSuccess x -> pure x
    DecafGraphqlQueryResultFailure v -> throwRequestException ("Error during parsing GraphQL response: " <> TL.toStrict (TLE.decodeUtf8 (Aeson.encode v)))


-- | Convenience function for 'runDecafGraphql' that hits DECAF Microlot API
-- namespace.
runDecafMicrolot
  :: HasCallStack
  => Aeson.FromJSON a
  => Aeson.ToJSON b
  => MonadCatch m
  => MonadThrow m
  => MonadIO m
  => String
  -> b
  -> DecafClient
  -> m a
runDecafMicrolot = runDecafGraphql apiMicrolot


-- | Convenience function for 'runDecafGraphqlNoVars' that hits DECAF Microlot
-- API namespace.
runDecafMicrolotNoVars
  :: HasCallStack
  => Aeson.FromJSON a
  => MonadCatch m
  => MonadThrow m
  => MonadIO m
  => String
  -> DecafClient
  -> m a
runDecafMicrolotNoVars = runDecafGraphqlNoVars apiMicrolot


-- | Convenience function for 'runDecafGraphql' that hits DECAF PDMS Module API
-- namespace.
runDecafModulePdms
  :: HasCallStack
  => Aeson.FromJSON a
  => Aeson.ToJSON b
  => MonadCatch m
  => MonadThrow m
  => MonadIO m
  => String
  -> b
  -> DecafClient
  -> m a
runDecafModulePdms = runDecafGraphql apiModulePdms


-- | Convenience function for 'runDecafGraphqlNoVars' that hits DECAF PDMS
-- Module API namespace.
runDecafModulePdmsNoVars
  :: HasCallStack
  => Aeson.FromJSON a
  => MonadCatch m
  => MonadThrow m
  => MonadIO m
  => String
  -> DecafClient
  -> m a
runDecafModulePdmsNoVars = runDecafGraphqlNoVars apiModulePdms
