{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}

module ExampleProfiles where

import           Control.Monad.IO.Class        (MonadIO(liftIO))
import qualified Data.Aeson.Yaml               as Aeson.Yaml
import qualified Data.Text                     as T
import           Decaf.Client.DecafCredentials
                 ( DecafBasicCredentials(..)
                 , DecafCredentials(..)
                 , DecafKeyCredentials(..)
                 )
import           Decaf.Client.DecafProfile     (DecafProfile(..))
import           Decaf.Client.DecafRemote      (DecafRemote(..))
import           Deriving.Aeson.Stock          (CustomJSON(CustomJSON), PrefixedSnake, ToJSON)
import           GHC.Generics                  (Generic)
import           GHC.Stack                     (HasCallStack)


runExampleProfiles
  :: HasCallStack
  => MonadIO m
  => FilePath
  -> m ()
runExampleProfiles _ = liftIO $ print (Aeson.Yaml.encode getExampleProfiles)


getExampleProfiles :: [DecafProfile]
getExampleProfiles =  [
  DecafProfile
    { decafProfileName = "example1"
    , decafProfileRemote = DecafRemote "example1.com" Nothing True
    , decafProfileCredentials = DecafCredentialsHeader "Header"
    },
  DecafProfile
    { decafProfileName = "example2"
    , decafProfileRemote = DecafRemote "example2.com" Nothing True
    , decafProfileCredentials = DecafCredentialsBasic (DecafBasicCredentials "username" "password")
    },
  DecafProfile
    { decafProfileName = "example3"
    , decafProfileRemote = DecafRemote "example3.com" Nothing True
    , decafProfileCredentials = DecafCredentialsKey (DecafKeyCredentials "key" "secret")
    }]
