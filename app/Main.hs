{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson                        (Value)
import qualified Data.Text                         as T
import           Decaf.Client
import           System.Environment                (getArgs)
import           System.Exit                       (die)


main :: IO ()
main = do
  args <- getArgs
  let eClient = mkDecafClient (T.pack $ args !! 0) (BasicCredentials (T.pack $ args !! 1) (T.pack $ args !! 2))
  case eClient of
    Left err     -> die $ err ++ "\nExiting..."
    Right (DecafClient barista microlot) -> do
      (print =<< (runBarista (path "version") barista :: IO (Response Value)))
      (print =<< (runMicrolot (mkMicrolotQuery' "query {\n principal {\nid\nusername\n} }") microlot :: IO (Response (MicrolotResponse Value))))
