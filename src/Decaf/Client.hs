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

  , module Decaf.Client.Internal.Remote

    -- * Requests

  , module Decaf.Client.DecafRequest

    -- * Responses

  , module Decaf.Client.DecafResponse

    -- * Credentials

  , module Decaf.Client.Internal.Credentials


    -- * Profiles

  , module Decaf.Client.Internal.Profile

    -- * Errors

  , module Decaf.Client.Internal.Error

    -- * Exceptions

  , module Decaf.Client.Internal.Exception

  ) where


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
import Decaf.Client.Internal.Credentials
import Decaf.Client.Internal.Error
import Decaf.Client.Internal.Exception
import Decaf.Client.Internal.Profile
import Decaf.Client.Internal.Remote
