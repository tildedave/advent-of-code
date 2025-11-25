module Util where

import Data.Char (isSpace)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- | pad
-- >>> pad 10 "boo"
-- "boo       "
-- >>> pad 3 "boom"
-- "boom"
pad :: Int -> String -> String
pad n s = if length s >= n then s else s ++ replicate (n - length s) ' '
