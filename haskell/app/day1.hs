{-# LANGUAGE OverloadedStrings #-}

module Day1 where

import Data.Char (isSpace)
import Data.List.Split (splitOn)
import Data.List (sortBy)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

part1 :: String -> Int
part1 = foldr (max . foldr ((+) . read) 0 . splitOn "\n") 0
    . splitOn "\n\n"
    . trim

part2 :: String -> Int
part2 = sum
    . take 3
    . sortBy (flip compare)
    . map (sum . map read . splitOn "\n")
    . splitOn "\n\n"
    . trim
