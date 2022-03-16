-- | This module provides definitions to work with DECAF client responses.

module Decaf.Client.DecafResponse where

import Network.HTTP.Types (ResponseHeaders, Status)


-- | Data definition for DECAF client response values.
data DecafResponse a = DecafResponse
  { decafResponseStatus  :: !Status
  , decafResponseHeaders :: !ResponseHeaders
  , decafResponseBody    :: !a
  }
  deriving Show
