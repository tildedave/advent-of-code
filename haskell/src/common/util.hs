module Util where

import Data.Char (isSpace)
import qualified Data.Map as M

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

-- | pad
-- >>> pad 10 "boo"
-- "boo       "
-- >>> pad 3 "boom"
-- "boom"
pad :: Int -> String -> String
pad n s = if length s >= n then s else s ++ replicate (n - length s) ' '

-- | modPositive
-- >>> modPositive (-5) 6
-- 1
-- >>> modPositive 13 7
-- 6
modPositive :: Int -> Int -> Int
modPositive n m =
  let r = n `mod` m
   in if r < 0 then modPositive (r + m) m else r

data Grid a = Grid
  { cells :: M.Map (Int, Int) a,
    bounds :: (Int, Int)
  }
