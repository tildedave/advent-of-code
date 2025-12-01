{-# LANGUAGE OverloadedStrings #-}

module Day3 where

import Data.Char (isLower, isUpper, ord)
import Data.List.Split (chunksOf)
import qualified Data.Set as Set
import qualified Data.Text as T

-- | parseRucksack
-- >>> parseRucksack (T.pack "vJrwpWtwJgWrhcsFMMfFFhFp")
-- ("vJrwpWtwJgWr","hcsFMMfFFhFp")
-- >>> parseRucksack (T.pack "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL")
-- ("jqHRNqRjqzjGDLGL","rsFMfFZSrLrFZsSL")
parseRucksack :: T.Text -> (T.Text, T.Text)
parseRucksack s = T.splitAt (div (T.length s) 2) s

rucksackChars :: T.Text -> Set.Set Char
rucksackChars = T.foldr Set.insert Set.empty

-- | commonChar
-- >>> commonChar (T.pack "vJrwpWtwJgWrhcsFMMfFFhFp")
-- 'p'
-- >>> commonChar (T.pack "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL")
-- 'L'
-- >>> commonChar (T.pack "PmmdzqPrVvPwwTWBwg")
-- 'P'
-- >>> commonChar (T.pack "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn")
-- 'v'
-- >>> commonChar (T.pack "ttgJtRGJQctTZtZT")
-- 't'
-- >>> commonChar (T.pack "CrZsJsPPZsGzwwsLwLmpwMDw")
-- 's'
commonChar :: T.Text -> Char
commonChar =
  Set.findMin
    . (\(r1, r2) -> Set.intersection (rucksackChars r1) (rucksackChars r2))
    . parseRucksack

priority :: Char -> Int
priority ch
  | isLower ch = ord ch - 96
  | isUpper ch = ord ch - 38 -- (-64 + 26) = 38
  | otherwise = error "invalid input"

-- | lineScore
-- >>> lineScore (T.pack "vJrwpWtwJgWrhcsFMMfFFhFp")
-- 16
-- >>> lineScore (T.pack "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL")
-- 38
-- >>> lineScore (T.pack "PmmdzqPrVvPwwTWBwg")
-- 42
-- >>> lineScore (T.pack "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn")
-- 22
-- >>> lineScore (T.pack "ttgJtRGJQctTZtZT")
-- 20
-- >>> lineScore (T.pack "CrZsJsPPZsGzwwsLwLmpwMDw")
-- 19
lineScore :: T.Text -> Int
lineScore = priority . commonChar

part1 :: T.Text -> Int
part1 = foldr ((+) . lineScore) 0 . T.splitOn "\n"

part2 :: T.Text -> Int
part2 =
  sum
    . map (priority . Set.findMin . foldr1 Set.intersection . map rucksackChars)
    . chunksOf 3
    . T.splitOn "\n"
