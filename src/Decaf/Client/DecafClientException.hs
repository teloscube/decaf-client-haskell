{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | This module provides definitions for exceptions "Decaf.Client" module can
-- throw and related helpers.
module Decaf.Client.DecafClientException where

import Control.Exception (Exception, IOException)
import Control.Monad.Catch (MonadThrow (throwM))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Decaf.Client.DecafRequest (DecafRequest)
import Decaf.Client.DecafResponse (DecafResponse)
import GHC.Stack (HasCallStack)
import qualified Network.HTTP.Simple as HS


-- | Type encoding of the exception that can be thrown by "Decaf.Client".
data DecafClientException where
  DecafClientIOException :: HasCallStack => T.Text -> IOException -> DecafClientException
  DecafClientParseException :: HasCallStack => T.Text -> T.Text -> DecafClientException
  DecafClientRemoteException :: HasCallStack => T.Text -> DecafClientException
  DecafClientRequestException :: HasCallStack => T.Text -> DecafClientException
  DecafClientHttpException :: HasCallStack => DecafRequest -> HS.HttpException -> DecafClientException
  DecafClientHttpStatusException :: HasCallStack => DecafResponse BL.ByteString -> DecafClientException


deriving instance Show DecafClientException


instance Exception DecafClientException


-- | Throws a 'DecafClientIOException' exception.
throwIOException
  :: HasCallStack
  => MonadThrow m
  => T.Text
  -- ^ Message.
  -> IOException
  -- ^ Underlying IO exception.
  -> m a
throwIOException msg exc = throwM (DecafClientIOException msg exc)


-- | Throws a 'DecafClientParseException' exception.
throwParseException
  :: HasCallStack
  => MonadThrow m
  => T.Text
  -- ^ Message.
  -> T.Text
  -- ^ Parse error.
  -> m a
throwParseException msg err = throwM (DecafClientParseException msg err)


-- | Throws a 'DecafClientRemoteException' exception.
throwRemoteException
  :: HasCallStack
  => MonadThrow m
  => T.Text
  -- ^ Message.
  -> m a
throwRemoteException = throwM . DecafClientRemoteException


-- | Throws a 'DecafClientRequestException' exception.
throwRequestException
  :: HasCallStack
  => MonadThrow m
  => T.Text
  -- ^ Message.
  -> m a
throwRequestException = throwM . DecafClientRequestException


-- | Throws a 'DecafClientHttpStatusException' exception.
throwStatusException
  :: HasCallStack
  => MonadThrow m
  => DecafResponse BL.ByteString
  -- ^ Raw-response that causes HTTP status exception.
  -> m a
throwStatusException = throwM . DecafClientHttpStatusException


-- | Throws a 'DecafClientHttpException' exception.
throwHttpException
  :: HasCallStack
  => MonadThrow m
  => DecafRequest
  -> HS.HttpException
  -> m a
throwHttpException request exception = throwM (DecafClientHttpException request exception)
