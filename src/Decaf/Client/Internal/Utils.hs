module Decaf.Client.Internal.Utils where


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


-- | Removes the trailing character from the given value.
--
-- >>> removeTrailingChar '/' ""
-- ""
-- >>> removeTrailingChar '/' "/"
-- ""
-- >>> removeTrailingChar '/' "/a"
-- "/a"
-- >>> removeTrailingChar '/' "/a/"
-- "/a"
-- >>> removeTrailingChar '/' "/a//"
-- "/a"
removeTrailingChar :: Char -> String -> String
removeTrailingChar _ [] = []
removeTrailingChar c x = if last x == c then removeTrailingChar c $ init x else x
