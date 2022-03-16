-- | This module provides high-level DECAF API client and related definitions.

{-# LANGUAGE RecordWildCards #-}

module Decaf.Client.DecafClient where

import           Control.Monad.Catch               (MonadCatch, MonadThrow)
import qualified Data.Text                         as T

import           Control.Monad.IO.Class            (MonadIO)
import qualified Data.Aeson                        as Aeson
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Lazy              as BL
import qualified Data.Text.Lazy                    as TL
import qualified Data.Text.Lazy.Encoding           as TLE
import           Decaf.Client.DecafClientException (throwRequestException)
import           Decaf.Client.DecafCredentials     (DecafCredentials)
import           Decaf.Client.DecafProfile         (DecafProfile(..))
import           Decaf.Client.DecafRemote          (DecafRemote, parseRemote)
import           Decaf.Client.DecafRequest
                 ( DecafGraphqlQuery
                 , DecafRequest(decafRequestRemote)
                 , DecafRequestCombinator
                 , apiBarista
                 , apiMicrolot
                 , apiModulePdms
                 , initRequest
                 , jsonPayload
                 )
import           Decaf.Client.DecafResponse
                 ( DecafGraphqlQueryResult(DecafGraphqlQueryResultFailure, DecafGraphqlQueryResultSuccess)
                 , DecafResponse(decafResponseBody)
                 )
import           Decaf.Client.Internal.Http
                 ( runRequestBL
                 , runRequestBS
                 , runRequestJson
                 , runRequestText
                 , runRequestVoid
                 )
import           GHC.Stack                         (HasCallStack)


-- * DECAF Client
-- $dataDefinition


-- | Data definition that represents a DECAF API client.
newtype DecafClient = DecafClient
  { unDecafClient :: DecafRequest
  }


-- | Returns the 'Remote' for the 'DecafClient'.
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
mkDecafClientM u c = flip mkDecafClient c <$> parseRemote u


-- | Builds a 'DecafClient' from the given 'Profile'.
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


runDecafRequestBS
  :: HasCallStack
  => MonadCatch m
  => MonadThrow m
  => Control.Monad.IO.Class.MonadIO m
  => DecafRequestCombinator
  -> DecafClient
  -> m (DecafResponse B.ByteString)
runDecafRequestBS c dc = runRequestBS (buildDecafRequest c dc)


runDecafRequestBL
  :: HasCallStack
  => MonadCatch m
  => MonadThrow m
  => Control.Monad.IO.Class.MonadIO m
  => DecafRequestCombinator
  -> DecafClient
  -> m (DecafResponse BL.ByteString)
runDecafRequestBL c dc = runRequestBL (buildDecafRequest c dc)


runDecafRequestText
  :: HasCallStack
  => MonadCatch m
  => MonadThrow m
  => Control.Monad.IO.Class.MonadIO m
  => DecafRequestCombinator
  -> DecafClient
  -> m (DecafResponse T.Text)
runDecafRequestText c dc = runRequestText (buildDecafRequest c dc)


runDecafRequestJson
  :: HasCallStack
  => Aeson.FromJSON a
  => MonadCatch m
  => MonadThrow m
  => Control.Monad.IO.Class.MonadIO m
  => DecafRequestCombinator
  -> DecafClient
  -> m (DecafResponse a)
runDecafRequestJson c dc = runRequestJson (buildDecafRequest c dc)


runDecafRequestVoid
  :: HasCallStack
  => MonadCatch m
  => MonadThrow m
  => Control.Monad.IO.Class.MonadIO m
  => DecafRequestCombinator
  -> DecafClient
  -> m (DecafResponse ())
runDecafRequestVoid c dc = runRequestVoid (buildDecafRequest c dc)


-- ** Barista Runners
-- $baristaRunners


runDecafBaristaRequestBS
  :: HasCallStack
  => MonadCatch m
  => MonadThrow m
  => Control.Monad.IO.Class.MonadIO m
  => DecafRequestCombinator
  -> DecafClient
  -> m B.ByteString
runDecafBaristaRequestBS c = fmap decafResponseBody . runDecafRequestBS (c . apiBarista)


runDecafBaristaRequestBL
  :: HasCallStack
  => MonadCatch m
  => MonadThrow m
  => Control.Monad.IO.Class.MonadIO m
  => DecafRequestCombinator
  -> DecafClient
  -> m BL.ByteString
runDecafBaristaRequestBL c = fmap decafResponseBody . runDecafRequestBL (c . apiBarista)


runDecafBaristaRequestText
  :: HasCallStack
  => MonadCatch m
  => MonadThrow m
  => Control.Monad.IO.Class.MonadIO m
  => DecafRequestCombinator
  -> DecafClient
  -> m T.Text
runDecafBaristaRequestText c = fmap decafResponseBody . runDecafRequestText (c . apiBarista)


runDecafBaristaRequestJson
  :: HasCallStack
  => Aeson.FromJSON a
  => MonadCatch m
  => MonadThrow m
  => Control.Monad.IO.Class.MonadIO m
  => DecafRequestCombinator
  -> DecafClient
  -> m a
runDecafBaristaRequestJson c = fmap decafResponseBody . runDecafRequestJson (c . apiBarista)


runDecafBaristaRequestVoid
  :: HasCallStack
  => MonadCatch m
  => MonadThrow m
  => Control.Monad.IO.Class.MonadIO m
  => DecafRequestCombinator
  -> DecafClient
  -> m ()
runDecafBaristaRequestVoid c = fmap decafResponseBody . runDecafRequestVoid (c . apiBarista)


-- ** Microlot Runners
-- $microlotRunners


runDecafMicrolotRequestJson
  :: HasCallStack
  => Show a
  => Aeson.FromJSON a
  => Aeson.ToJSON b
  => MonadCatch m
  => MonadThrow m
  => Control.Monad.IO.Class.MonadIO m
  => DecafGraphqlQuery b
  -> DecafClient
  -> m a
runDecafMicrolotRequestJson q dc = do
  value <- decafResponseBody <$> runDecafRequestJson (jsonPayload q . apiMicrolot) dc
  case value of
    DecafGraphqlQueryResultSuccess x -> pure x
    DecafGraphqlQueryResultFailure v -> throwRequestException ("Error during Microlot request: " <> TL.toStrict (TLE.decodeUtf8 (Aeson.encode v)))


-- ** PDMS Module Runners
-- $pdmsModuleRunners


runDecafModulePdmsRequestJson
  :: HasCallStack
  => Show a
  => Aeson.FromJSON a
  => Aeson.ToJSON b
  => MonadCatch m
  => MonadThrow m
  => Control.Monad.IO.Class.MonadIO m
  => DecafGraphqlQuery b
  -> DecafClient
  -> m a
runDecafModulePdmsRequestJson q dc = do
  value <- decafResponseBody <$> runDecafRequestJson (jsonPayload q . apiModulePdms) dc
  case value of
    DecafGraphqlQueryResultSuccess x -> pure x
    DecafGraphqlQueryResultFailure v -> throwRequestException ("Error during PDMS Module request: " <> TL.toStrict (TLE.decodeUtf8 (Aeson.encode v)))
