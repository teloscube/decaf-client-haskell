-- | @TODO: Provide documentation.@
--
module Decaf.Client
  ( -- * Barista
    BaristaClient
  , mkBaristaClient
  , runBarista
  , runBaristaBS

    -- * Microlot

  , MicrolotClient
  , mkMicrolotClient
  , runMicrolot
  , mkMicrolotQuery
  , mkMicrolotQuery'
  , MicrolotQuery(..)
  , MicrolotResponse(..)

    -- * Common Types

  , module Decaf.Client.Internal.Types

    -- * Request Combinators

  , module Decaf.Client.Internal.Combinators
  ) where

-- TODO: Re-export top-level definitions.

import Decaf.Client.Internal.Barista
import Decaf.Client.Internal.Combinators hiding (dummyDef)
import Decaf.Client.Internal.Microlot
import Decaf.Client.Internal.Types hiding (mkPath)
