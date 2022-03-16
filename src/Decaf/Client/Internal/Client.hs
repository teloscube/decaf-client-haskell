-- | This module provides 'DecafClient' definition and related helpers.

module Decaf.Client.Internal.Client where

import           Control.Monad.Error.Class           (MonadError)
import qualified Data.Text                           as T
import           Decaf.Client.Internal.Apis.Barista  (BaristaClient, mkBaristaClient)
import           Decaf.Client.Internal.Apis.Microlot (MicrolotClient, mkMicrolotClient)
import           Decaf.Client.Internal.Apis.Pdms     (PdmsClient, mkPdmsClient)
import           Decaf.Client.Internal.Credentials   (Credentials)
import           Decaf.Client.Internal.Error         (DecafClientError)
import           Decaf.Client.Internal.Remote        (Remote, parseRemote)


-- | Data definition for a collection of various DECAF API clients.
data DecafClient = DecafClient
  { decafClientRemote   :: !Remote          -- ^ DECAF remote definition for the DECAF Instance.
  , decafClientBarista  :: !BaristaClient   -- ^ DECAF Barista API client.
  , decafClientMicrolot :: !MicrolotClient  -- ^ DECAF Microlot API client.
  , decafClientPdms     :: !PdmsClient      -- ^ DECAF PDMS Module API client.
  }


-- | Builds a 'DecafClient' with given DECAF Instance 'Remote' and
-- authentication credentials.
mkDecafClient
  :: Remote       -- ^ DECAF Instance 'Remote'.
  -> Credentials  -- ^ Credentials for authenticating requests to remote DECAF Instance
  -> DecafClient
mkDecafClient r c =
  let
    barista = mkBaristaClient r c
    microlot = mkMicrolotClient r c
    pdms = mkPdmsClient r c
  in
    DecafClient r barista microlot pdms


-- | Attempts to build a 'DecafClient' with given remote DECAF Instance URL
-- and authentication credentials.
mkDecafClientE
  :: MonadError DecafClientError m
  => T.Text       -- ^ Base URL of remote DECAF Instance
  -> Credentials  -- ^ Credentials for authenticating requests to remote DECAF Instance
  -> m DecafClient
mkDecafClientE b c = do
  r <- parseRemote b
  let barista = mkBaristaClient r c
  let microlot = mkMicrolotClient r c
  let pdms = mkPdmsClient r c
  pure (DecafClient r barista microlot pdms)
