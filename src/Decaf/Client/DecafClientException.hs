-- | This module provides definitions for exceptions "Decaf.Client" module can
-- throw and related helpers.

module Decaf.Client.DecafClientException where

import           Control.Exception   (Exception, IOException)
import           Control.Monad.Catch (MonadThrow(throwM))
import qualified Data.Text           as T
import           GHC.Stack           (HasCallStack)


-- | Type encoding of the exception that can be thrown by "Decaf.Client".
data DecafClientException where
  DecafClientIOException :: HasCallStack => T.Text -> IOException -> DecafClientException
  DecafClientParseException :: HasCallStack => T.Text -> T.Text -> DecafClientException


deriving instance Show DecafClientException


instance Exception DecafClientException


-- | Throws a 'DecafClientIOException' exception.
throwIOException
  :: HasCallStack
  => MonadThrow m
  => T.Text       -- ^ Message.
  -> IOException  -- ^ Underlying IO exception.
  -> m a
throwIOException msg exc = throwM (DecafClientIOException msg exc)


-- | Throws a 'DecafClientParseException' exception.
throwParseException
  :: HasCallStack
  => MonadThrow m
  => T.Text  -- ^ Message.
  -> T.Text  -- ^ Parse error.
  -> m a
throwParseException msg err = throwM (DecafClientParseException msg err)
