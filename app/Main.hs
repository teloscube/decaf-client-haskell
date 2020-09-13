module Main where

import qualified Data.ByteString.Char8         as BC
import           Decaf.Client.Internal.Barista (get, mkBaristaClient, mkPath)
import           Decaf.Client.Internal.Http    (Credentials(BasicCredentials), mkBaseUrl)
import           System.Environment            (getArgs)


main :: IO ()
main = do
  args <- getArgs
  let client = mkBaristaClient (mkBaseUrl $ args !! 0) (BasicCredentials (BC.pack $ args !! 1) (BC.pack $ args !! 2))
  BC.putStrLn =<< get [] (mkPath ["version"]) [] client
