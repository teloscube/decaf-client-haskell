-- | This module provides data definitions and auxiliaries to build and work
-- with 'Response' values.

module Decaf.Client.Internal.Response where

import Decaf.Client.Internal.Request (Headers)


-- | Data definition for DECAF API response values.
data Response a = Response
  { responseStatus  :: !Int
  , responseHeaders :: Headers
  , responseValue   :: !a
  }
  deriving Show
