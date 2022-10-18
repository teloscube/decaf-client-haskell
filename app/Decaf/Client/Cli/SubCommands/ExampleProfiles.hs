{-# LANGUAGE OverloadedStrings #-}

module Decaf.Client.Cli.SubCommands.ExampleProfiles where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString as B
import qualified Data.Yaml as Yaml
import qualified Decaf.Client as DC
import GHC.Stack (HasCallStack)


runExampleProfiles
  :: HasCallStack
  => MonadIO m
  => m ()
runExampleProfiles = liftIO $ B.putStr (Yaml.encode getExampleProfiles)


getExampleProfiles :: [DC.DecafProfile]
getExampleProfiles =
  [ DC.DecafProfile
      { DC.decafProfileName = "example1"
      , DC.decafProfileRemote = DC.DecafRemote "example1.com" Nothing True
      , DC.decafProfileCredentials = DC.DecafCredentialsHeader "Header"
      }
  , DC.DecafProfile
      { DC.decafProfileName = "example2"
      , DC.decafProfileRemote = DC.DecafRemote "example2.com" Nothing True
      , DC.decafProfileCredentials = DC.DecafCredentialsBasic (DC.DecafBasicCredentials "username" "password")
      }
  , DC.DecafProfile
      { DC.decafProfileName = "example3"
      , DC.decafProfileRemote = DC.DecafRemote "example3.com" Nothing True
      , DC.decafProfileCredentials = DC.DecafCredentialsKey (DC.DecafKeyCredentials "key" "secret")
      }
  , DC.DecafProfile
      { DC.decafProfileName = "example4"
      , DC.decafProfileRemote = DC.DecafRemote "example4.com" Nothing True
      , DC.decafProfileCredentials = DC.DecafCredentialsToken "authToken"
      }
  ]
