{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (pack, stripEnd)
import Day1 (part1, part2)
import Day2 (part1, part2)
import Day3 (part1, part2)
import System.Environment
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  contents <- stripEnd . pack <$> readFile (args !! 2)
  case take 2 args of
    ["day1", "1"] -> printf "%d\n" (Day1.part1 contents)
    ["day1", "2"] -> printf "%d\n" (Day1.part2 contents)
    ["day2", "1"] -> printf "%d\n" (Day2.part1 contents)
    ["day2", "2"] -> printf "%d\n" (Day2.part2 contents)
    ["day3", "1"] -> printf "%d\n" (Day3.part1 contents)
    ["day3", "2"] -> printf "%d\n" (Day3.part2 contents)
    _ -> do
      putStrLn "Not found"
