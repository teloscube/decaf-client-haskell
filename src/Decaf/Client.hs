-- | @TODO: Provide documentation.@
--
module Decaf.Client
  ( -- * Client

    DecafClient(..)
  , mkDecafClient
  , mkDecafClientE

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

  , module Decaf.Client.DecafRemote

    -- * Requests

  , module Decaf.Client.DecafRequest

    -- * Responses

  , module Decaf.Client.DecafResponse

    -- * Credentials

  , module Decaf.Client.DecafCredentials


    -- * Profiles

  , module Decaf.Client.DecafProfile

    -- * Errors

  , module Decaf.Client.Internal.Error

    -- * Exceptions

  , module Decaf.Client.DecafClientException

  ) where


import Decaf.Client.DecafClientException
import Decaf.Client.DecafCredentials
import Decaf.Client.DecafProfile
import Decaf.Client.DecafRemote
import Decaf.Client.DecafRequest
import Decaf.Client.DecafResponse
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
import Decaf.Client.Internal.Client        (DecafClient(..), mkDecafClient, mkDecafClientE)
import Decaf.Client.Internal.Error
