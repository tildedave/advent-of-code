{-# LANGUAGE OverloadedStrings #-}

module Day3 where

import Data.Char (digitToInt)
import Data.List (intercalate, sortBy)
import qualified Data.Text as T

-- | highestVoltage
-- >>> highestVoltage "987654321111111"
-- 98
highestVoltage :: T.Text -> Int
highestVoltage s =
  let h = maximum (init l)
   in let rest = dropWhile (< h) l
       in h * 10 + maximum (drop 1 rest)
  where
    l = map digitToInt $ T.unpack s

part1 :: T.Text -> Int
part1 = sum . map highestVoltage . T.splitOn "\n"

-- part2 is harder
-- really it is just looking for, of all 12 digit substrings, which is the maximum
-- I wonder if we can use a descent approach
-- look for position of the 9.  if the remainder of the string has 11+
-- characters, it's the "pivot", repeat with the rest.  otherwise go on to 8
-- can cache this too
-- unfortunate that I am doing this in Haskell
-- create char -> positions map

textPositions :: T.Text -> [(Int, Int)]
textPositions s =
  sortBy
    ( \(n, x) (n', y) ->
        case compare y x of
          EQ -> compare n' n
          b -> b
    )
    $ zip [T.length s - 1, T.length s - 2 .. 0]
    $ map digitToInt
    $ T.unpack s

-- OK this should work
highestSuffix :: [(Int, Int)] -> Int -> [Int]
highestSuffix _ 0 = []
highestSuffix m n =
  let (nextStart, nextDigit) = head $ filter (\(x, _) -> x >= n - 1) m
   in (nextDigit : highestSuffix (filter (\(x, _) -> x < nextStart) m) (n - 1))

-- | highestVoltagePart2
-- >>> highestVoltagePart2 "818181911112111"
-- 888911112111
highestVoltagePart2 :: T.Text -> Int
highestVoltagePart2 t =
  let m = textPositions t
   in read $ intercalate "" (map show (highestSuffix m 12))

part2 :: T.Text -> Int
part2 = sum . map highestVoltagePart2 . T.splitOn "\n"
