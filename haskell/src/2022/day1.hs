{-# LANGUAGE OverloadedStrings #-}

module Day1 where

import qualified Data.Text as T
import Data.Text (stripEnd, splitOn, unpack)

-- import Data.List.Split (splitOn)
import Data.List (sortBy)

part1 :: T.Text -> Int
part1 = foldr (max . foldr ((+) . read . unpack) 0 . splitOn "\n") 0
    . splitOn "\n\n"
    . stripEnd

part2 :: T.Text -> Int
part2 = sum
    . take 3
    . sortBy (flip compare)
    . map (sum . map (read . unpack) . splitOn "\n")
    . splitOn "\n\n"
    . stripEnd
