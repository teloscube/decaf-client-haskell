-- | This module provides generic auxiliaries.

module Decaf.Client.Internal.Utils where

import qualified Data.Aeson as Aeson
import           Data.List  (dropWhileEnd, stripPrefix)
import           Data.Maybe (fromMaybe)


-- | Removes leading, successive elements from a list of elements if it is equal
-- to the pivot value given.
--
-- >>> dropLeading ':' ""
-- ""
-- >>> dropLeading ':' ":"
-- ""
-- >>> dropLeading ':' "::"
-- ""
-- >>> dropLeading ':' ":a:"
-- "a:"
-- >>> dropLeading ':' "::a::"
-- "a::"
dropLeading :: Eq a => a -> [a] -> [a]
dropLeading c = dropWhile (c ==)


-- | Removes trailing, successive elements from a list of elements if it is
-- equal to the pivot value given.
--
-- >>> dropTrailing '/' ""
-- ""
-- >>> dropTrailing '/' "/"
-- ""
-- >>> dropTrailing '/' "/a"
-- "/a"
-- >>> dropTrailing '/' "/a/"
-- "/a"
-- >>> dropTrailing '/' "/a//"
-- "/a"
dropTrailing :: Eq a => a -> [a] -> [a]
dropTrailing c = dropWhileEnd (c ==)


-- | Returns @'Just' x@ if @x@ is a non-empty string, 'Nothing' otherwise.
--
-- >>> nonEmptyString ""
-- Nothing
-- >>> nonEmptyString " "
-- Just " "
nonEmptyString :: String -> Maybe String
nonEmptyString x = if x == "" then Nothing else Just x


-- | Composes a list of functions.
compose :: [a -> a] -> a -> a
compose = foldl (flip (.)) id


-- | Common Aeson encoding/decoding options.
commonAesonOptions :: String -> Aeson.Options
commonAesonOptions prefix =
  Aeson.defaultOptions
    { Aeson.omitNothingFields = True
    , Aeson.fieldLabelModifier = \l -> Aeson.camelTo2 '_' . fromMaybe l $ stripPrefix prefix l
    , Aeson.constructorTagModifier = \l -> Aeson.camelTo2 '_' . fromMaybe l $ stripPrefix prefix l
    , Aeson.sumEncoding = Aeson.TaggedObject
        { Aeson.tagFieldName = "type"
        , Aeson.contentsFieldName = "value"
        }
    }
