-- | This module provides types and functions to transcode DECAF API requests.

{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE DerivingVia #-}

module Decaf.Client.DecafCredentials where

import qualified Data.Char            as C
import qualified Data.Text            as T
import qualified Deriving.Aeson       as DA
import qualified Deriving.Aeson.Stock as DAS


-- | Data definition for available DECAF credentials types.
--
-- >>> Data.Aeson.encode (DecafCredentialsHeader "some-header-value")
-- "{\"type\":\"header\",\"value\":\"some-header-value\"}"
-- >>> Data.Aeson.encode (DecafCredentialsBasic (DecafBasicCredentials "some-username" "some-password"))
-- "{\"type\":\"basic\",\"value\":{\"username\":\"some-username\",\"password\":\"some-password\"}}"
-- >>> Data.Aeson.encode (DecafCredentialsKey (DecafKeyCredentials "some-api-key" "some-api-secret"))
-- "{\"type\":\"key\",\"value\":{\"key\":\"some-api-key\",\"secret\":\"some-api-secret\"}}"
-- >>> Data.Aeson.encode (DecafCredentialsToken "some-api-token")
-- "{\"type\":\"token\",\"value\":\"some-api-token\"}"
data DecafCredentials =
    DecafCredentialsHeader !T.Text
  | DecafCredentialsBasic !DecafBasicCredentials
  | DecafCredentialsKey !DecafKeyCredentials
  | DecafCredentialsToken !T.Text
  deriving (Eq, DA.Generic)
  deriving (DA.FromJSON, DA.ToJSON) via DA.CustomJSON '[DA.ConstructorTagModifier '[DA.StripPrefix "DecafCredentials", FirstToLower], DA.SumTaggedObject "type" "value"] DecafCredentials


instance Show DecafCredentials where
  show _ = "<********>"


-- | Data definition for HTTP basic authentication credentials.
--
-- >>> let x = DecafBasicCredentials "some-username" "some-password"
-- >>> Data.Aeson.encode x
-- "{\"username\":\"some-username\",\"password\":\"some-password\"}"
-- >>> Just x == Data.Aeson.decode @DecafBasicCredentials (Data.Aeson.encode x)
-- True
data DecafBasicCredentials = DecafBasicCredentials
  { decafBasicCredentialsUsername :: !T.Text
  , decafBasicCredentialsPassword :: !T.Text
  }
  deriving (Eq, DA.Generic)
  deriving (DA.FromJSON, DA.ToJSON) via DAS.PrefixedSnake "decafBasicCredentials" DecafBasicCredentials


-- | Data definition for HTTP basic authentication credentials.
--
-- >>> let x = DecafKeyCredentials "some-api-key" "some-api-secret"
-- >>> Data.Aeson.encode x
-- "{\"key\":\"some-api-key\",\"secret\":\"some-api-secret\"}"
-- >>> Just x == Data.Aeson.decode @DecafKeyCredentials (Data.Aeson.encode x)
-- True
data DecafKeyCredentials = DecafKeyCredentials
  { decafKeyCredentialsKey    :: !T.Text
  , decafKeyCredentialsSecret :: !T.Text
  }
  deriving (Eq, DA.Generic)
  deriving (DA.FromJSON, DA.ToJSON) via DAS.PrefixedSnake "decafKeyCredentials" DecafKeyCredentials


-- * Internal
-- $internal


-- | Data definition for string modifier that lowers the first character.
data FirstToLower


instance DA.StringModifier FirstToLower where
  getStringModifier []       = []
  getStringModifier (x : xs) = C.toLower x : xs
