-- | This module provides library version information.
--
module Decaf.Client.Version where

import           Data.Version       (showVersion)
import qualified Paths_decaf_client as P


-- | Package version.
--
-- >>> version == showVersion P.version
-- True
version :: String
version = showVersion P.version
