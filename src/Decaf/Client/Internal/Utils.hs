-- | This module provides generic auxiliaries.
--
module Decaf.Client.Internal.Utils where

import Data.List (dropWhileEnd)


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


-- | Returns @Just x@ if @x@ is a non-empty string, @Nothing@ otherwise.
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


-- | Applies a function to the first element of a list.
--
-- >>> applyFirst id [] :: [Int]
-- []
-- >>> applyFirst (1 +) [0, 2, 3] :: [Int]
-- [1,2,3]
applyFirst :: (a -> a) -> [a] -> [a]
applyFirst _ []     = []
applyFirst f (x:xs) = f x : xs
