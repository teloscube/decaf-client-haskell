-- | This module provides a DECAF Barista API client implementation.
--
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}

module Decaf.Client.Internal.Barista where

import           Data.Aeson                        (FromJSON)
import qualified Data.ByteString                   as B
import qualified Data.Text                         as T
import qualified Decaf.Client.Internal.Combinators as IC
import qualified Decaf.Client.Internal.Http        as IH
import qualified Decaf.Client.Internal.Request     as IR
import qualified Decaf.Client.Internal.Types       as IT


-- | DECAF Barista API client type.
newtype BaristaClient = MkBaristaClient { unBaristaClient :: IT.Request } deriving Show


-- | Attempts to build a 'BaristaClient' with the given DECAF deployment and credentials information.
--
-- >>> mkClient "https://example.com" (IT.HeaderCredentials "OUCH")
-- Right (MkBaristaClient {unBaristaClient = Request {
--   requestHost              = "example.com"
--   requestPort              = Nothing
--   requestNamespace         = MkPath {unPath = ["api"]}
--   requestIsSecure          = True
--   requestCredentials       = <********>
--   requestUserAgent         = "DECAF API Client/0.0.0.1 (Haskell)"
--   requestHttpHeaders       = [("X-DECAF-URL","https://example.com")]
--   requestHttpMethod        = GET
--   requestHttpPath          = MkPath {unPath = []}
--   requestHttpTrailingSlash = True
--   requestHttpParams        = []
--   requestHttpPayload       = Nothing
-- }
-- })
mkClient :: T.Text -> IT.Credentials -> Either String BaristaClient
mkClient d c = MkBaristaClient . IC.namespace "api" . IC.withTrailingSlash <$> IR.initRequest d c


runClient :: IC.Combinator -> BaristaClient -> IO B.ByteString
runClient cmb cli = IH.runRequest $ mkRequest cmb cli

runClient' :: FromJSON a => IC.Combinator -> BaristaClient -> IO a
runClient' cmb cli = IH.runRequest' $ mkRequest cmb cli


--------------------
-- BEGIN INTERNAL --
--------------------


-- | Builds an 'IT.Request' from a 'BaristaClient' while applying a 'IC.Combinator'.
mkRequest :: IC.Combinator -> BaristaClient -> IT.Request
mkRequest c = c . unBaristaClient


------------------
-- END INTERNAL --
------------------
