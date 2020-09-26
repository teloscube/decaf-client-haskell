{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson          (Value)
import qualified Data.Text           as T
import           Decaf.Client
                 ( Credentials(BasicCredentials)
                 , DecafClient(DecafClient)
                 , MicrolotResponse
                 , Response
                 , mkDecafClient
                 , mkMicrolotQuery'
                 , path
                 , runBarista
                 , runMicrolot
                 )
import           Options.Applicative
                 ( Parser
                 , execParser
                 , fullDesc
                 , header
                 , help
                 , helper
                 , info
                 , long
                 , metavar
                 , progDesc
                 , short
                 , strOption
                 , (<**>)
                 )
import           System.Exit         (die)


main :: IO ()
main = program =<< mkDecafClient' =<< execParser opts
 where
   opts = info
     (parser <**> helper)
     ( fullDesc
     <> progDesc "Demo Application for DECAF Haskell Client Library"
     <> header "THIS IS A DEMO APPLICATION" )


program :: DecafClient -> IO ()
program (DecafClient barista microlot) = do
  print =<< (runBarista (path "version") barista :: IO (Response Value))
  print =<< (runMicrolot (mkMicrolotQuery' "query {\n principal {\nid\nusername\n} }") microlot :: IO (Response (MicrolotResponse Value)))


mkDecafClient' :: (T.Text, T.Text, T.Text) -> IO DecafClient
mkDecafClient' (d, u, p) = case mkDecafClient d (BasicCredentials u p) of
  Left err -> die $ show err ++ "\nExiting..."
  Right dc -> pure dc


parser :: Parser (T.Text, T.Text, T.Text)
parser = (,,)
  <$> strOption (short 'd' <> long "deployment" <> metavar "DEPLOYMENT" <> help "Deployment URL")
  <*> strOption (short 'u' <> long "username" <> metavar "USERNAME" <> help "Username")
  <*> strOption (short 'p' <> long "password" <> metavar "PASSWORD" <> help "Password")
