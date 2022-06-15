{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Mocking where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import qualified Data.Aeson                  as Aeson
import qualified Data.ByteString.Lazy        as BL
import qualified Data.HashMap.Strict         as HM
import qualified Data.Text                   as T
import           Decaf.Client
import           Decaf.Client.Internal.Utils (commonAesonOptions)
import           GHC.Generics                (Generic)
import           GHC.Stack


-- * Mock Client
-- $client


mockClient :: DecafClient
mockClient = mkDecafClient mockRemote mockCredentials


mockRemote :: DecafRemote
mockRemote = DecafRemote
    { decafRemoteHost   = "httpbin.org"
    , decafRemotePort   = Nothing
    , decafRemoteSecure = True
    }


mockCredentials :: DecafCredentials
mockCredentials = DecafCredentialsBasic $ DecafBasicCredentials
  { decafBasicCredentialsUsername = "hebele"
  , decafBasicCredentialsPassword = "hubele"
  }


-- * Anything
-- $anything


data MockAnythingResponseBody = MockAnythingResponseBody
  { mockAnythingResponseBodyArgs    :: !(HM.HashMap T.Text (Maybe T.Text))
  , mockAnythingResponseBodyData    :: !T.Text
  , mockAnythingResponseBodyFiles   :: !(HM.HashMap T.Text Aeson.Value)
  , mockAnythingResponseBodyForm    :: !(HM.HashMap T.Text (Maybe T.Text))
  , mockAnythingResponseBodyHeaders :: !(HM.HashMap T.Text (Maybe T.Text))
  , mockAnythingResponseBodyJson    :: !Aeson.Value
  , mockAnythingResponseBodyMethod  :: !T.Text
  , mockAnythingResponseBodyOrigin  :: !T.Text
  , mockAnythingResponseBodyUrl     :: !T.Text
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
  => DecafRequestCombinator
  -> m (DecafResponse BL.ByteString)
runMockRequest =
  performDecafRequest . flip buildDecafRequest mockClient


runMockRequestJson
  :: HasCallStack
  => Aeson.FromJSON a
  => MonadIO m
  => MonadCatch m
  => MonadThrow m
  => DecafRequestCombinator
  -> m (DecafResponse a)
runMockRequestJson =
  performDecafRequestJson . flip buildDecafRequest mockClient
