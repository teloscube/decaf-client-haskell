module Decaf.Client.Cli.SubCommands.ExampleProfiles where

import           Control.Monad.IO.Class        (MonadIO(liftIO))
import qualified Data.ByteString               as B
import qualified Data.Text                     as T
import qualified Data.Yaml                     as Yaml
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
  => m ()
runExampleProfiles = liftIO $ B.putStr (Yaml.encode getExampleProfiles)


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
    },
  DecafProfile
    { decafProfileName = "example4"
    , decafProfileRemote = DecafRemote "example4.com" Nothing True
    , decafProfileCredentials = DecafCredentialsToken "authToken"
    }]
