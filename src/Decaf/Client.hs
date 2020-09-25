-- | @TODO: Provide documentation.@
--
module Decaf.Client
  ( -- * Client

    DecafClient(..)
  , mkDecafClient

    -- * Barista Client

  , BaristaClient
  , mkBaristaClient
  , runBarista
  , runBaristaBS

    -- * Microlot Client

  , MicrolotClient
  , mkMicrolotClient
  , runMicrolot
  , mkMicrolotQuery
  , mkMicrolotQuery'
  , MicrolotQuery(..)
  , MicrolotResponse(..)

    -- * Request Combinators

  , module Decaf.Client.Internal.Combinators

    -- * Common Types

  , module Decaf.Client.Internal.Types

  ) where


import Data.Text                         (Text)
import Decaf.Client.Internal.Barista
import Decaf.Client.Internal.Combinators hiding (dummyDef)
import Decaf.Client.Internal.Microlot
import Decaf.Client.Internal.Types       hiding (mkPath)


-- | Data definition for a collection of various DECAF API clients.
data DecafClient = DecafClient
  { decafClientBarista  :: !BaristaClient   -- ^ DECAF Barista API client.
  , decafClientMicrolot :: !MicrolotClient  -- ^ DECAF Microlot API client.
  }


-- | Attempts to build a 'DecafClient' with given remote DECAF deployment URL
-- and authentication credentials.
mkDecafClient :: Text -> Credentials -> Either String DecafClient
mkDecafClient d c = do
  baristaClient <- mkBaristaClient d c
  microlotClient <- mkMicrolotClient d c
  Right $ DecafClient baristaClient microlotClient
