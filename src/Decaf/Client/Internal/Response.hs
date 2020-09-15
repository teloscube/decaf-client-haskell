-- | This module provides DECAF response definitions.
--
module Decaf.Client.Internal.Response where

import Decaf.Client.Internal.Types (Headers)


-- | Data definition for DECAF API response values.
data Response a = Response
  { responseStatus  :: !Int
  , responseHeaders :: Headers
  , responseValue   :: !a
  } deriving Show
