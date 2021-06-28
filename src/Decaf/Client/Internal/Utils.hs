-- | This module provides generic auxiliaries.
--
module Decaf.Client.Internal.Utils where

import Data.ByteString (ByteString)
import Data.List       (dropWhileEnd)


-- | Splits the 'String' by the given predicate.
--
-- >>> splitWhen (== '/') ""
-- []
-- >>> splitWhen (== '/') "a"
-- ["a"]
-- >>> splitWhen (== '/') "/a"
-- ["a"]
-- >>> splitWhen (== '/') "a/"
-- ["a"]
-- >>> splitWhen (== '/') "a/b"
-- ["a","b"]
-- >>> splitWhen (== '/') "a//b//c"
-- ["a","b","c"]
splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen p s =  case dropWhile p s of
  "" -> []
  s' -> w : splitWhen p s''
    where (w, s'') = break p s'


-- | Removes leading, successive elements from a list of elements if it is equal to the pivot value given.
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


-- | Removes trailing, successive elements from a list of elements if it is equal to the pivot value given.
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


-- | Converts a 'Maybe' to an 'Either'.
--
-- >>> maybeToEither "Can not parse an integer" Nothing :: Either String Int
-- Left "Can not parse an integer"
-- >>> maybeToEither "Can not parse an integer" (Just 1) :: Either String Int
-- Right 1
maybeToEither :: e -> Maybe a -> Either e a
maybeToEither = flip maybe Right . Left


-- | Returns @Just x@ if @x@ is a non-empty string, @Nothing@ otherwise.
--
-- >>> nonEmptyString ""
-- Nothing
-- >>> nonEmptyString " "
-- Just " "
nonEmptyString :: String -> Maybe String
nonEmptyString x = if x == "" then Nothing else Just x


-- | Returns @Just x@ if @x@ is a non-empty string, @Nothing@ otherwise.
--
-- >>> nonEmptyByteString ""
-- Nothing
-- >>> nonEmptyByteString " "
-- Just " "
nonEmptyByteString :: ByteString -> Maybe ByteString
nonEmptyByteString x = if x == "" then Nothing else Just x


-- | Composes a list of functions.
compose :: [a -> a] -> a -> a
compose = foldl (flip (.)) id


-- | Applies a function to the first element of a list.
--
-- >>> applyFirst id [] :: [Int]
-- []
-- >>> applyFirst (1 +) [0, 2, 3] :: [Int]
-- [1,2,3]
applyFirst :: (a -> a) -> [a] -> [a]
applyFirst _ []     = []
applyFirst f (x:xs) = f x : xs
