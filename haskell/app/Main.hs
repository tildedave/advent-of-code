module Main where

import System.Environment
import Text.Printf (printf)

import Day1

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (args !! 2)
    case take 2 args of
        ["day1", "1"] -> printf "%d\n" (Day1.part1 contents)
        ["day1", "2"] -> printf "%d\n" (Day1.part2 contents)
        _ -> do
            putStrLn "Not found"
