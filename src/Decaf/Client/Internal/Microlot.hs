-- | This module provides a DECAF Microlot client implementation.
--
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}

module Decaf.Client.Internal.Microlot where

import           Data.Aeson                        (FromJSON, ToJSON(..), Value, object, (.=))
import qualified Data.Text                         as T
import qualified Decaf.Client.Internal.Combinators as IC
import qualified Decaf.Client.Internal.Http        as IH
import qualified Decaf.Client.Internal.Request     as IR
import qualified Decaf.Client.Internal.Types       as IT


-- | DECAF Microlot API client type.
newtype MicrolotClient = MkMicrolotClient { unMicrolotClient :: IT.Request } deriving Show


-- | Attempts to build a 'MicrolotClient' with the given DECAF deployment and credentials information.
--
-- >>> mkClient "https://example.com" (IT.HeaderCredentials "OUCH")
-- Right (MkMicrolotClient {unMicrolotClient = Request {
--   requestHost              = "example.com"
--   requestPort              = Nothing
--   requestNamespace         = MkPath {unPath = ["apis","microlot","v1","graphql"]}
--   requestIsSecure          = True
--   requestCredentials       = <********>
--   requestUserAgent         = "DECAF API Client/0.0.0.1 (Haskell)"
--   requestHttpHeaders       = [("X-DECAF-URL","https://example.com")]
--   requestHttpMethod        = POST
--   requestHttpPath          = MkPath {unPath = []}
--   requestHttpTrailingSlash = False
--   requestHttpParams        = []
--   requestHttpPayload       = Nothing
-- }
-- })
mkClient :: T.Text -> IT.Credentials -> Either String MicrolotClient
mkClient d c = MkMicrolotClient . IC.post . IC.namespace "/apis/microlot/v1/graphql" . IC.withoutTrailingSlash <$> IR.initRequest d c


runClient :: (ToJSON a, FromJSON b) => MicrolotQuery a -> MicrolotClient -> IO b
runClient query cli = IH.runRequest' $ mkRequest (IC.jsonPayload query) cli


-- | Microlot query type as a sealed query/variables tuple.
data MicrolotQuery a = ToJSON a => MkMicrolotQuery !String !a

instance ToJSON (MicrolotQuery a) where
  toJSON (MkMicrolotQuery q v) = object ["query" .= q, "variables" .= v]


mkMicrolotQuery :: ToJSON a => String -> a -> MicrolotQuery a
mkMicrolotQuery = MkMicrolotQuery


mkMicrolotQuery' :: String -> MicrolotQuery Value
mkMicrolotQuery' = flip MkMicrolotQuery $ object []


--------------------
-- BEGIN INTERNAL --
--------------------


-- | Builds an 'IT.Request' from a 'MicrolotClient' while applying a 'IC.Combinator'.
mkRequest :: IC.Combinator -> MicrolotClient -> IT.Request
mkRequest c = c . unMicrolotClient


------------------
-- END INTERNAL --
------------------
