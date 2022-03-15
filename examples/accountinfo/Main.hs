{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
                 ( FromJSON(..)
                 , Options(fieldLabelModifier)
                 , defaultOptions
                 , genericParseJSON
                 )
import           Data.Char                   (toLower)
import qualified Data.Text                   as T
import           Decaf.Client
                 ( Credentials(..)
                 , DecafClient
                 , KeyCredentials(..)
                 , decafClientBarista
                 , decafClientMicrolot
                 , decafClientPdms
                 , microlotResponseData
                 , mkDecafClient
                 , mkMicrolotQuery'
                 , mkPdmsQuery'
                 , path
                 , pdmsResponseData
                 , responseValue
                 , runBarista
                 , runMicrolot
                 , runPdms
                 )
import           Decaf.Client.Internal.Utils (applyFirst)
import           GHC.Generics                (Generic)
import           System.Environment          (getEnv)
import           System.IO                   (hPutStrLn, stderr)
import           Text.Printf                 (printf)


main :: IO ()
main = do
  apiurl <- T.pack <$> getEnv "DECAF_API_URL"
  apikey <- T.pack <$> getEnv "DECAF_API_KEY"
  apiscr <- T.pack <$> getEnv "DECAF_API_SECRET"
  let credentials = CredentialsKey (KeyCredentials apikey apiscr)
  case mkDecafClient apiurl credentials of
    Left err -> hPutStrLn stderr $ "Can not create DECAF client" <> show err
    Right dc -> do
      -- printRemoteVersion dc
      -- printAccounts dc
      printPolicies dc


data VersionResponse = VersionResponse
  { version :: !String
  } deriving (Generic, Show)

instance FromJSON VersionResponse


printRemoteVersion :: DecafClient -> IO ()
printRemoteVersion client = do
  let barista = decafClientBarista client
  response <- runBarista (path "version") barista
  let message = "Remote API version is " ++ (version . responseValue $ response)
  putStrLn message


data AccountsResponse = AccountsResponse
  { accounts :: ![AccountsResponseItem]
  } deriving (Generic, Show)

instance FromJSON AccountsResponse


data AccountsResponseItem = AccountsResponseItem
  { id   :: !Int
  , name :: !T.Text
  } deriving (Generic, Show)

instance FromJSON AccountsResponseItem


printAccounts :: DecafClient -> IO ()
printAccounts client = do
  let microlot = decafClientMicrolot client
  response <- runMicrolot (mkMicrolotQuery' "query {\n accounts: account {\nid\nname\n} }") microlot
  let accs = accounts . microlotResponseData . responseValue $ response
  putStrLn "Accounts:"
  mapM_ printAccount (enumerate accs)
  where
    printAccount (count, AccountsResponseItem id name) = putStrLn $ printf "%d. [%d] %s" count id (T.unpack name)


enumerate :: [a] -> [(Int, a)]
enumerate = zip [1..]


data PolicyResponse = PolicyResponse
  { policies :: ![PolicyResponseItem]
  } deriving (Generic, Show)

instance FromJSON PolicyResponse


data PolicyResponseItem = PolicyResponseItem
  { policyId   :: !T.Text
  , policyCode :: !T.Text
  } deriving (Generic, Show)

instance FromJSON PolicyResponseItem where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = applyFirst toLower . drop 6 }

printPolicies :: DecafClient -> IO ()
printPolicies client = do
  let pdms = decafClientPdms client
  response <- runPdms (mkPdmsQuery' "query {\n policies: policy {\nid\ncode\n} }") pdms
  let pols = policies . pdmsResponseData . responseValue $ response
  putStrLn "Policies:"
  mapM_ printPolicy (enumerate pols)
  where
    printPolicy (count, PolicyResponseItem policyid code) = putStrLn $ printf "%d. [%s] %s" count policyid (T.unpack code)
