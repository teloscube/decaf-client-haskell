module Main where

import qualified Data.ByteString.Char8         as BC
import           Decaf.Client.Internal.Barista (get, mkBaristaClient)
import           Decaf.Client.Internal.Http    (Credentials(BasicCredentials))
import           System.Environment            (getArgs)


main :: IO ()
main = do
  args <- getArgs
  let client = mkBaristaClient (args !! 0) (BasicCredentials (BC.pack $ args !! 1) (BC.pack $ args !! 2))
  BC.putStrLn =<< get [] ["version"] [] client
