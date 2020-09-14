{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson                        (Value)
import qualified Data.Text                         as T
import qualified Decaf.Client.Internal.Barista     as Barista
import           Decaf.Client.Internal.Combinators (Combinator, path)
import qualified Decaf.Client.Internal.Microlot    as Microlot
import           Decaf.Client.Internal.Types       (Credentials(BasicCredentials))
import           System.Environment                (getArgs)
import           System.Exit                       (die)


main :: IO ()
main = do
  args <- getArgs
  let ebclient = Barista.mkClient (T.pack $ args !! 0) (BasicCredentials (T.pack $ args !! 1) (T.pack $ args !! 2))
  case ebclient of
    Left err     -> die $ err ++ "\nExiting..."
    Right client -> print client >> (print =<< (Barista.runClient' baristaVersion client :: IO Value))

  let emclient = Microlot.mkClient (T.pack $ args !! 0) (BasicCredentials (T.pack $ args !! 1) (T.pack $ args !! 2))
  case emclient of
    Left err     -> die $ err ++ "\nExiting..."
    Right client -> print client >> (print =<< (Microlot.runClient microlotPrincipals client :: IO Value))


microlotPrincipals :: Microlot.MicrolotQuery Value
microlotPrincipals = Microlot.mkMicrolotQuery' "query {\n principal {\nid\nusername\n} }"


baristaVersion :: Combinator
baristaVersion = path "version"
