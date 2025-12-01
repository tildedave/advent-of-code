{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import qualified Data.Text as T

type Range = (Int, Int)

parseRange :: T.Text -> Range
parseRange s = case T.splitOn "-" s of
  [lower, upper] -> (read (T.unpack lower), read (T.unpack upper))
  _ -> error "bad input"

-- | parseLine
-- >>> parseLine (T.pack "2-4,6-8")
-- ((2,4),(6,8))
parseLine :: T.Text -> (Range, Range)
parseLine s =
  case T.splitOn "," s of
    [range1Str, range2Str] -> (parseRange range1Str, parseRange range2Str)
    _ -> error "bad input"

fullyContained :: Range -> Range -> Bool
fullyContained (s1, e1) (s2, e2) = (s1 <= s2 && e2 <= e1) || (s2 <= s1 && e1 <= e2)

part1 :: T.Text -> Int
part1 = length . filter (uncurry fullyContained . parseLine) . T.splitOn "\n"

overlaps :: Range -> Range -> Bool
overlaps (s1, e1) (s2, e2) = (s2 <= s1 && s1 <= e2) || (s1 <= s2 && s2 <= e1)

part2 :: T.Text -> Int
part2 = length . filter (uncurry overlaps . parseLine) . T.splitOn "\n"
