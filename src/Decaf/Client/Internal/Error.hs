-- | This module provides DECAF client error data type and related definitions.
--
-- Note that currently we are working under 'MonadError'. Eventually, we are
-- going to move onto 'MonadThrow'. Therefore, this module will be replaced with
-- some "Exceptions" module.

module Decaf.Client.Internal.Error where

import Control.Monad.Except (MonadError(throwError))


-- | Type definition for DECAF client errors.
newtype DecafClientError = DecafClientError
  { unDecafClientError :: String
  } deriving (Show)


-- | Throws a DECAF client error.
--
-- >>> throwDecafClientError "Invalid request" :: Either DecafClientError ()
-- Left (DecafClientError {unDecafClientError = "Invalid request"})
throwDecafClientError :: MonadError DecafClientError m => String -> m a
throwDecafClientError = throwError . DecafClientError
