-- | This module provides combinators to modify 'Request' values.
--
{-# LANGUAGE OverloadedStrings #-}

module Decaf.Client.Internal.Combinators where

import           Data.Aeson                  (ToJSON, encode)
import qualified Data.ByteString.Lazy        as BL
import qualified Data.Text                   as T
import           Decaf.Client.Internal.Types
                 ( Credentials
                 , Header
                 , Headers
                 , Method(..)
                 , Param
                 , Params
                 , Path
                 , Payload(Payload)
                 , Remote
                 , Request(..)
                 , mkPath
                 )


-- | Type definition of 'I.Request' combinator.
type Combinator = Request -> Request


setRemote :: Remote -> Combinator
setRemote h request = request { requestRemote = h }


remote :: Remote -> Combinator
remote = setRemote


setNamespace :: Path -> Combinator
setNamespace n request = request { requestNamespace = n }


namespace :: T.Text -> Combinator
namespace = setNamespace . mkPath


setCredentials :: Credentials -> Combinator
setCredentials c request = request { requestCredentials = c }


credentials :: Credentials -> Combinator
credentials = setCredentials


setUserAgent :: T.Text -> Combinator
setUserAgent ua request = request { requestUserAgent = ua}


userAgent :: T.Text -> Combinator
userAgent = setUserAgent


setHeaders :: Headers -> Combinator
setHeaders hs request = request { requestHttpHeaders = hs }


addHeaders :: Headers -> Combinator
addHeaders hs request = setHeaders (existing ++ hs) request
  where
    headkeys = fmap fst hs
    existing = filter (\x -> fst x `notElem` headkeys) . requestHttpHeaders $ request


headers :: Headers -> Combinator
headers = setHeaders


addHeader :: Header -> Combinator
addHeader h = addHeaders [h]


header :: T.Text -> T.Text -> Combinator
header k v = addHeader (k, v)


setMethod :: Method -> Combinator
setMethod m request = request { requestHttpMethod = m }


get :: Combinator
get = setMethod GET


post :: Combinator
post = setMethod POST


put :: Combinator
put = setMethod PUT


delete :: Combinator
delete = setMethod DELETE


setPath :: Path -> Combinator
setPath p request = request { requestHttpPath = p}


addPath :: Path -> Combinator
addPath p request = setPath (requestHttpPath request <> p) request


path :: T.Text -> Combinator
path = addPath . mkPath


setTrailingSlash :: Bool -> Combinator
setTrailingSlash ts request = request { requestHttpTrailingSlash = ts }


withTrailingSlash :: Combinator
withTrailingSlash = setTrailingSlash True


withoutTrailingSlash :: Combinator
withoutTrailingSlash = setTrailingSlash False


setParams :: Params -> Combinator
setParams ps request = request { requestHttpParams = ps}


addParams :: Params -> Combinator
addParams ps request = setParams (requestHttpParams request ++ ps) request


params :: Params -> Combinator
params = addParams


addParam :: Param -> Combinator
addParam p = addParams [p]


param :: Param -> Combinator
param = addParam


setPayload :: T.Text -> BL.ByteString -> Combinator
setPayload t c request = request { requestHttpPayload = Just $ Payload t c }


payload :: T.Text -> BL.ByteString -> Combinator
payload = setPayload


jsonPayload :: ToJSON a => a -> Combinator
jsonPayload x = setPayload "application/json" $ encode x


setNoPayload :: Combinator
setNoPayload request = request { requestHttpPayload = Nothing }


noPayload :: Combinator
noPayload = setNoPayload


-- TODO: See and follow https://github.com/haskell/haddock/issues/958
dummyDef :: ()
dummyDef = undefined
