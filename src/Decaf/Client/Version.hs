-- | This module provides library version information.
--
module Decaf.Client.Version where

import           Data.Version       (showVersion)
import qualified Paths_decaf_client as P


-- | Package version.
--
-- >>> version
-- "0.0.0.2"
version :: String
version = showVersion P.version
