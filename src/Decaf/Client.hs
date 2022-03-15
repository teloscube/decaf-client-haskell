-- | @TODO: Provide documentation.@
--
module Decaf.Client
  ( -- * Client

    DecafClient(..)
  , mkDecafClient

    -- * Barista Client

  , BaristaClient
  , mkBaristaClient
  , mkBaristaClientM
  , runBarista
  , runBaristaBS

    -- * Microlot Client

  , MicrolotClient
  , mkMicrolotClient
  , mkMicrolotClientM
  , runMicrolot
  , mkMicrolotQuery
  , mkMicrolotQuery'
  , MicrolotQuery(..)
  , MicrolotResponse(..)

    -- * Pdms Client

  , PdmsClient
  , mkPdmsClient
  , mkPdmsClientM
  , runPdms
  , mkPdmsQuery
  , mkPdmsQuery'
  , PdmsQuery(..)
  , PdmsResponse(..)

    -- * Remotes

  , module Decaf.Client.Internal.Remote

    -- * Requests

  , module Decaf.Client.Internal.Request

    -- * Responses

  , module Decaf.Client.Internal.Response

    -- * Credentials

  , module Decaf.Client.Internal.Credentials


    -- * Profiles

  , module Decaf.Client.Internal.Profile

    -- * Errors

  , module Decaf.Client.Internal.Error

    -- * Exceptions

  , module Decaf.Client.Internal.Exception

  ) where


import Control.Monad.Except                (MonadError)
import Data.Text                           (Text)
import Decaf.Client.Internal.Apis.Barista  (BaristaClient, mkBaristaClient, mkBaristaClientM, runBarista, runBaristaBS)
import Decaf.Client.Internal.Apis.Microlot
       ( MicrolotClient
       , MicrolotQuery(..)
       , MicrolotResponse(..)
       , mkMicrolotClient
       , mkMicrolotClientM
       , mkMicrolotQuery
       , mkMicrolotQuery'
       , runMicrolot
       )
import Decaf.Client.Internal.Apis.Pdms
       ( PdmsClient
       , PdmsQuery(..)
       , PdmsResponse(..)
       , mkPdmsClient
       , mkPdmsClientM
       , mkPdmsQuery
       , mkPdmsQuery'
       , runPdms
       )
import Decaf.Client.Internal.Credentials
import Decaf.Client.Internal.Error
import Decaf.Client.Internal.Exception
import Decaf.Client.Internal.Profile
import Decaf.Client.Internal.Remote
import Decaf.Client.Internal.Request
import Decaf.Client.Internal.Response


-- | Data definition for a collection of various DECAF API clients.
data DecafClient = DecafClient
  { decafClientRemote   :: !Remote          -- ^ DECAF remote definition for the DECAF Instance.
  , decafClientBarista  :: !BaristaClient   -- ^ DECAF Barista API client.
  , decafClientMicrolot :: !MicrolotClient  -- ^ DECAF Microlot API client.
  , decafClientPdms     :: !PdmsClient      -- ^ DECAF PDMS Module API client.
  }


-- | Attempts to build a 'DecafClient' with given remote DECAF Instance URL
-- and authentication credentials.
mkDecafClient
  :: MonadError DecafClientError m
  => Text            -- ^ Base URL of remote DECAF Instance
  -> Credentials     -- ^ Credentials for authenticating requests to remote DECAF Instance
  -> m DecafClient
mkDecafClient b c = do
  r <- parseRemote b
  let barista = mkBaristaClient r c
  let microlot = mkMicrolotClient r c
  let pdms = mkPdmsClient r c
  pure (DecafClient r barista microlot pdms)
