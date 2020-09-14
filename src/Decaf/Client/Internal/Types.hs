-- | This module provides types and functions to transcode DECAF API requests.
--
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module Decaf.Client.Internal.Types where

import qualified Data.ByteString.Lazy        as BL
import qualified Data.Text                   as T
import           Decaf.Client.Internal.Utils (dropTrailing)


data Request = Request
  { requestHost              :: !T.Text
  , requestPort              :: !(Maybe Int)
  , requestNamespace         :: !Path
  , requestIsSecure          :: !Bool
  , requestCredentials       :: !Credentials
  , requestUserAgent         :: !T.Text
  , requestHttpHeaders       :: !Headers
  , requestHttpMethod        :: !Method
  , requestHttpPath          :: !Path
  , requestHttpTrailingSlash :: !Bool
  , requestHttpParams        :: ![Param]
  , requestHttpPayload       :: !(Maybe Payload)
  }

instance Show Request where
  show x = dropTrailing '\n' $ unlines
    [ "Request {"
    , "  requestHost              = " ++ show (requestHost x)
    , "  requestPort              = " ++ show (requestPort x)
    , "  requestNamespace         = " ++ show (requestNamespace x)
    , "  requestIsSecure          = " ++ show (requestIsSecure x)
    , "  requestCredentials       = " ++ show (requestCredentials x)
    , "  requestUserAgent         = " ++ show (requestUserAgent x)
    , "  requestHttpHeaders       = " ++ show (requestHttpHeaders x)
    , "  requestHttpMethod        = " ++ show (requestHttpMethod x)
    , "  requestHttpPath          = " ++ show (requestHttpPath x)
    , "  requestHttpTrailingSlash = " ++ show (requestHttpTrailingSlash x)
    , "  requestHttpParams        = " ++ show (requestHttpParams x)
    , "  requestHttpPayload       = " ++ show (requestHttpPayload x)
    , "}"
    ]


type Header = (T.Text, T.Text)


type Headers = [Header]


type Param = (T.Text, T.Text)


type Params = [Param]


newtype Path = MkPath { unPath :: [T.Text] } deriving Show

-- >>> mkPath "/a/b" <> mkPath "/c/d"
-- MkPath {unPath = ["a","b","c","d"]}
instance Semigroup Path where
  (<>) (MkPath p1) (MkPath p2) = MkPath $ p1 ++ p2

-- >>> mconcat [mkPath "/a/b", mkPath "/c/d"]
-- MkPath {unPath = ["a","b","c","d"]}
instance Monoid Path where
  mempty = MkPath []
  mappend = (<>)
  mconcat ps = MkPath $ concatMap unPath ps


-- Sanitizes the given 'T.Text' into 'PathSegment' values and builds a 'Path' value.
--
-- >>> mkPath ""
-- MkPath {unPath = []}
-- >>> mkPath "/"
-- MkPath {unPath = []}
-- >>> mkPath "//"
-- MkPath {unPath = []}
-- >>> mkPath "/a"
-- MkPath {unPath = ["a"]}
-- >>> mkPath "a/"
-- MkPath {unPath = ["a"]}
-- >>> mkPath "/a/"
-- MkPath {unPath = ["a"]}
-- >>> mkPath "/a/b"
-- MkPath {unPath = ["a","b"]}
-- >>> mkPath "a/b/"
-- MkPath {unPath = ["a","b"]}
-- >>> mkPath "/a/b/"
-- MkPath {unPath = ["a","b"]}
mkPath :: T.Text -> Path
mkPath = MkPath . filter ("" /=) . T.split ('/' ==)


-- | Data definition for available DECAF credentials types.
data Credentials =
    HeaderCredentials !T.Text
  | BasicCredentials !T.Text !T.Text
  | KeyCredentials !T.Text !T.Text
  | TokenCredentials !T.Text

instance Show Credentials where
  show _ = "<********>"


-- | Available DECAF endpoint methods.
data Method = GET | POST | PUT | DELETE deriving (Show)


data Payload = Payload
  { payloadType    :: !T.Text
  , payloadContent :: !BL.ByteString
  }

instance Show Payload where
  show _ = "<TRUNCATED>"
