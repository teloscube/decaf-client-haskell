{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson                        (Value)
import qualified Data.Text                         as T
import           Decaf.Client.Internal.Barista     (mkBaristaClient, runBarista)
import           Decaf.Client.Internal.Combinators (Combinator, path)
import           Decaf.Client.Internal.Microlot
                 ( MicrolotQuery
                 , MicrolotResponse
                 , mkMicrolotClient
                 , mkMicrolotQuery'
                 , runMicrolot
                 )
import           Decaf.Client.Internal.Types       (Credentials(BasicCredentials), Response)
import           System.Environment                (getArgs)
import           System.Exit                       (die)


main :: IO ()
main = do
  args <- getArgs
  let deployment = (T.pack $ args !! 0)
  let username = (T.pack $ args !! 1)
  let password = (T.pack $ args !! 2)
  let credentials = (BasicCredentials username password)
  let ebclient = mkBaristaClient deployment credentials
  case ebclient of
    Left err     -> die $ err ++ "\nExiting..."
    Right client -> print client >> (print =<< (runBarista baristaVersion client :: IO (Response Value)))

  let emclient = mkMicrolotClient deployment credentials
  case emclient of
    Left err     -> die $ err ++ "\nExiting..."
    Right client -> print client >> (print =<< (runMicrolot microlotPrincipals client :: IO (Response (MicrolotResponse Value))))


microlotPrincipals :: MicrolotQuery Value
microlotPrincipals = mkMicrolotQuery' "query {\n principal {\nid\nusername\n} }"


baristaVersion :: Combinator
baristaVersion = path "version"
