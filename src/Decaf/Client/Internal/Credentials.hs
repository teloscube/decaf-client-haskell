-- | This module provides types and functions to transcode DECAF API requests.

{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE DerivingVia #-}

module Decaf.Client.Internal.Credentials where

import qualified Data.Text            as T
import qualified Deriving.Aeson       as DA
import qualified Deriving.Aeson.Stock as DAS


-- | Data definition for available DECAF credentials types.
--
-- >>> Data.Aeson.encode (CredentialsHeader "some-header-value")
-- "{\"type\":\"Header\",\"value\":\"some-header-value\"}"
-- >>> Data.Aeson.encode (CredentialsBasic (BasicCredentials "some-username" "some-password"))
-- "{\"type\":\"Basic\",\"value\":{\"username\":\"some-username\",\"password\":\"some-password\"}}"
-- >>> Data.Aeson.encode (CredentialsKey (KeyCredentials "some-api-key" "some-api-secret"))
-- "{\"type\":\"Key\",\"value\":{\"key\":\"some-api-key\",\"secret\":\"some-api-secret\"}}"
-- >>> Data.Aeson.encode (CredentialsToken "some-api-token")
-- "{\"type\":\"Token\",\"value\":\"some-api-token\"}"
data Credentials =
    CredentialsHeader !T.Text
  | CredentialsBasic !BasicCredentials
  | CredentialsKey !KeyCredentials
  | CredentialsToken !T.Text
  deriving (Eq, DA.Generic)
  deriving (DA.FromJSON, DA.ToJSON) via DA.CustomJSON '[DA.ConstructorTagModifier (DA.StripPrefix "Credentials"), DA.SumTaggedObject "type" "value"] Credentials


instance Show Credentials where
  show _ = "<********>"


-- | Data definition for HTTP basic authentication credentials.
--
-- >>> let x = BasicCredentials "some-username" "some-password"
-- >>> Data.Aeson.encode x
-- "{\"username\":\"some-username\",\"password\":\"some-password\"}"
-- >>> Just x == Data.Aeson.decode @BasicCredentials (Data.Aeson.encode x)
-- True
data BasicCredentials = BasicCredentials
  { basicCredentialsUsername :: !T.Text
  , basicCredentialsPassword :: !T.Text
  }
  deriving (Eq, DA.Generic)
  deriving (DA.FromJSON, DA.ToJSON) via DAS.PrefixedSnake "basicCredentials" BasicCredentials


-- | Data definition for HTTP basic authentication credentials.
--
-- >>> let x = KeyCredentials "some-api-key" "some-api-secret"
-- >>> Data.Aeson.encode x
-- "{\"key\":\"some-api-key\",\"secret\":\"some-api-secret\"}"
-- >>> Just x == Data.Aeson.decode @KeyCredentials (Data.Aeson.encode x)
-- True
data KeyCredentials = KeyCredentials
  { keyCredentialsKey    :: !T.Text
  , keyCredentialsSecret :: !T.Text
  }
  deriving (Eq, DA.Generic)
  deriving (DA.FromJSON, DA.ToJSON) via DAS.PrefixedSnake "keyCredentials" KeyCredentials
