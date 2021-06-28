-- | This module provides a DECAF Barista API client implementation.
--
module Decaf.Client.Internal.Barista where

import           Control.Monad.IO.Class            (MonadIO)
import           Data.Aeson                        (FromJSON)
import qualified Data.ByteString                   as B
import qualified Data.Text                         as T
import qualified Decaf.Client.Internal.Combinators as IC
import qualified Decaf.Client.Internal.Http        as IH
import qualified Decaf.Client.Internal.Remote      as IRemote
import qualified Decaf.Client.Internal.Request     as IR
import qualified Decaf.Client.Internal.Types       as IT


-- | DECAF Barista API client type.
newtype BaristaClient = MkBaristaClient { unBaristaClient :: IT.Request } deriving Show


-- | Builds a 'BaristaClient' with the given DECAF deployment 'IRemote.Remote' and credentials.
--
-- >>> mkBaristaClient (IT.Remote "example.com" Nothing True) (IT.HeaderCredentials "OUCH") :: BaristaClient
-- MkBaristaClient {unBaristaClient = Request {
--   requestRemote            = [https]://[example.com]:[443]
--   requestNamespace         = MkPath {unPath = ["api"]}
--   requestCredentials       = <********>
--   requestUserAgent         = "DECAF API Client/0.0.0.2 (Haskell)"
--   requestHttpHeaders       = [("X-DECAF-URL","https://example.com:443")]
--   requestHttpMethod        = GET
--   requestHttpPath          = MkPath {unPath = []}
--   requestHttpTrailingSlash = True
--   requestHttpParams        = []
--   requestHttpPayload       = Nothing
-- }}
mkBaristaClient :: IT.Remote -> IT.Credentials -> BaristaClient
mkBaristaClient r c = MkBaristaClient . IC.namespace "api" . IC.withTrailingSlash $ IR.initRequest r c


-- | Attempts to build a 'BaristaClient' with the given DECAF deployment URL and credentials.
--
-- >>> mkBaristaClientM "https://example.com" (IT.HeaderCredentials "OUCH") :: Either IT.DecafClientError BaristaClient
-- Right (MkBaristaClient {unBaristaClient = Request {
--   requestRemote            = [https]://[example.com]:[443]
--   requestNamespace         = MkPath {unPath = ["api"]}
--   requestCredentials       = <********>
--   requestUserAgent         = "DECAF API Client/0.0.0.2 (Haskell)"
--   requestHttpHeaders       = [("X-DECAF-URL","https://example.com:443")]
--   requestHttpMethod        = GET
--   requestHttpPath          = MkPath {unPath = []}
--   requestHttpTrailingSlash = True
--   requestHttpParams        = []
--   requestHttpPayload       = Nothing
-- }})
mkBaristaClientM :: IT.DecafClientM m => T.Text -> IT.Credentials -> m BaristaClient
mkBaristaClientM d c = (`mkBaristaClient` c) <$> IRemote.parseRemote d


-- | Runs the 'BaristaClient' along with given 'IR.Request' combinators and
-- returns a 'IV.Response' value JSON-decoded from the response body.
runBarista :: (MonadIO m, FromJSON a) => IC.Combinator -> BaristaClient -> m (IT.Response a)
runBarista cmb cli = IH.runRequest $ mkRequest cmb cli


-- | Runs the 'BaristaClient' along with given 'IR.Request' combinators and
-- returns a 'IT.Response' with 'B.ByteString' value.
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
