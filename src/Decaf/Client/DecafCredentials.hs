{-# LANGUAGE DeriveGeneric #-}

-- | This module provides types and functions to transcode DECAF API requests.
module Decaf.Client.DecafCredentials where

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import Decaf.Client.Internal.Utils (commonAesonOptions)
import GHC.Generics (Generic)


-- | Data definition for available DECAF credentials types.
--
-- >>> Data.Aeson.encode (DecafCredentialsHeader "some-header-value")
-- "{\"type\":\"header\",\"value\":\"some-header-value\"}"
-- >>> Data.Aeson.encode (DecafCredentialsBasic (DecafBasicCredentials "some-username" "some-password"))
-- "{\"type\":\"basic\",\"value\":{\"password\":\"some-password\",\"username\":\"some-username\"}}"
-- >>> Data.Aeson.encode (DecafCredentialsKey (DecafKeyCredentials "some-api-key" "some-api-secret"))
-- "{\"type\":\"key\",\"value\":{\"key\":\"some-api-key\",\"secret\":\"some-api-secret\"}}"
-- >>> Data.Aeson.encode (DecafCredentialsToken "some-api-token")
-- "{\"type\":\"token\",\"value\":\"some-api-token\"}"
data DecafCredentials
  = DecafCredentialsHeader !T.Text
  | DecafCredentialsBasic !DecafBasicCredentials
  | DecafCredentialsKey !DecafKeyCredentials
  | DecafCredentialsToken !T.Text
  deriving (Eq, Generic)


instance Aeson.FromJSON DecafCredentials where
  parseJSON = Aeson.genericParseJSON $ commonAesonOptions "DecafCredentials"


instance Aeson.ToJSON DecafCredentials where
  toJSON = Aeson.genericToJSON $ commonAesonOptions "DecafCredentials"


instance Show DecafCredentials where
  show _ = "<********>"


-- | Data definition for HTTP basic authentication credentials.
--
-- >>> let x = DecafBasicCredentials "some-username" "some-password"
-- >>> Data.Aeson.encode x
-- "{\"password\":\"some-password\",\"username\":\"some-username\"}"
-- >>> Just x == Data.Aeson.decode @DecafBasicCredentials (Data.Aeson.encode x)
-- True
data DecafBasicCredentials = DecafBasicCredentials
  { decafBasicCredentialsUsername :: !T.Text
  , decafBasicCredentialsPassword :: !T.Text
  }
  deriving (Eq, Generic)


instance Aeson.FromJSON DecafBasicCredentials where
  parseJSON = Aeson.genericParseJSON $ commonAesonOptions "decafBasicCredentials"


instance Aeson.ToJSON DecafBasicCredentials where
  toJSON = Aeson.genericToJSON $ commonAesonOptions "decafBasicCredentials"


-- | Data definition for HTTP basic authentication credentials.
--
-- >>> let x = DecafKeyCredentials "some-api-key" "some-api-secret"
-- >>> Data.Aeson.encode x
-- "{\"key\":\"some-api-key\",\"secret\":\"some-api-secret\"}"
-- >>> Just x == Data.Aeson.decode @DecafKeyCredentials (Data.Aeson.encode x)
-- True
data DecafKeyCredentials = DecafKeyCredentials
  { decafKeyCredentialsKey :: !T.Text
  , decafKeyCredentialsSecret :: !T.Text
  }
  deriving (Eq, Generic)


instance Aeson.FromJSON DecafKeyCredentials where
  parseJSON = Aeson.genericParseJSON $ commonAesonOptions "decafKeyCredentials"


instance Aeson.ToJSON DecafKeyCredentials where
  toJSON = Aeson.genericToJSON $ commonAesonOptions "decafKeyCredentials"
