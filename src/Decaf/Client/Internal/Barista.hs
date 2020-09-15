-- | This module provides a DECAF Barista API client implementation.
--
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}

module Decaf.Client.Internal.Barista where

import           Control.Monad.IO.Class            (MonadIO)
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
-- >>> mkBaristaClient "https://example.com" (IT.HeaderCredentials "OUCH")
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
-- }})
mkBaristaClient :: T.Text -> IT.Credentials -> Either String BaristaClient
mkBaristaClient d c = MkBaristaClient . IC.namespace "api" . IC.withTrailingSlash <$> IR.initRequest d c


-- | Runs the 'BaristaClient' along with given 'IR.Request' combinators and
-- returns a 'IV.Response' value JSON-decoded from the response body.
runBarista :: (MonadIO m, FromJSON a) => IC.Combinator -> BaristaClient -> m (IT.Response a)
runBarista cmb cli = IH.runRequest $ mkRequest cmb cli


-- | Runs the 'BaristaClient' along with given 'IR.Request' combinators and
-- returns a 'Response' with 'B.ByteString' value.
runBaristaBS :: MonadIO m => IC.Combinator -> BaristaClient -> m (IT.Response B.ByteString)
runBaristaBS cmb cli = IH.runRequestBS $ mkRequest cmb cli


--------------------
-- BEGIN INTERNAL --
--------------------


-- | Builds an 'IT.Request' from a 'BaristaClient' while applying a 'IC.Combinator'.
mkRequest :: IC.Combinator -> BaristaClient -> IT.Request
mkRequest c = c . unBaristaClient


------------------
-- END INTERNAL --
------------------
