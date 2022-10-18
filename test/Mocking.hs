{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Mocking where

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Decaf.Client as DC
import Decaf.Client.Internal.Utils (commonAesonOptions)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)


-- * Mock Client


mockClient :: DC.DecafClient
mockClient = DC.mkDecafClient mockRemote mockCredentials


mockRemote :: DC.DecafRemote
mockRemote =
  DC.DecafRemote
    { DC.decafRemoteHost = "httpbin.org"
    , DC.decafRemotePort = Nothing
    , DC.decafRemoteSecure = True
    }


mockCredentials :: DC.DecafCredentials
mockCredentials =
  DC.DecafCredentialsBasic $
    DC.DecafBasicCredentials
      { DC.decafBasicCredentialsUsername = "hebele"
      , DC.decafBasicCredentialsPassword = "hubele"
      }


-- * Anything


data MockAnythingResponseBody = MockAnythingResponseBody
  { mockAnythingResponseBodyArgs :: !(HM.HashMap T.Text (Maybe T.Text))
  , mockAnythingResponseBodyData :: !T.Text
  , mockAnythingResponseBodyFiles :: !(HM.HashMap T.Text Aeson.Value)
  , mockAnythingResponseBodyForm :: !(HM.HashMap T.Text (Maybe T.Text))
  , mockAnythingResponseBodyHeaders :: !(HM.HashMap T.Text (Maybe T.Text))
  , mockAnythingResponseBodyJson :: !Aeson.Value
  , mockAnythingResponseBodyMethod :: !T.Text
  , mockAnythingResponseBodyOrigin :: !T.Text
  , mockAnythingResponseBodyUrl :: !T.Text
  }
  deriving (Generic, Show)


instance Aeson.FromJSON MockAnythingResponseBody where
  parseJSON = Aeson.genericParseJSON $ commonAesonOptions "mockAnythingResponseBody"


instance Aeson.ToJSON MockAnythingResponseBody where
  toJSON = Aeson.genericToJSON $ commonAesonOptions "mockAnythingResponseBody"


runMockRequest
  :: HasCallStack
  => MonadIO m
  => MonadCatch m
  => MonadThrow m
  => DC.DecafRequestCombinator
  -> m (DC.DecafResponse BL.ByteString)
runMockRequest =
  DC.performDecafRequest . flip DC.buildDecafRequest mockClient


runMockRequestJson
  :: HasCallStack
  => Aeson.FromJSON a
  => MonadIO m
  => MonadCatch m
  => MonadThrow m
  => DC.DecafRequestCombinator
  -> m (DC.DecafResponse a)
runMockRequestJson =
  DC.performDecafRequestJson . flip DC.buildDecafRequest mockClient
