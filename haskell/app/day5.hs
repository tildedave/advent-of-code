{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day5 where

import qualified Data.Text as T
import Data.Maybe (mapMaybe, fromJust)
import Text.Read (readMaybe)
import qualified Data.Map as Map
import Data.Map ((!))
import Util (trim)

import Data.List (uncons, foldl')
import Data.List.Split (splitOn)

-- input is awful but oh well

type Instruction = [Int]
parseInstruction :: T.Text -> Instruction
parseInstruction = mapMaybe readMaybe . words . T.unpack

-- boxes are at 1  5  9  ...
parseBoxLine :: T.Text -> [Char]
parseBoxLine = (\s -> map (s !!) [1,5..length s]) . T.unpack

parseBoxes :: [T.Text] -> Map.Map Int [Char]
parseBoxes =
    snd .
    foldl' (\(n, m) s -> (n + 1, Map.insert n (trim s) m)) (1, Map.empty) .
    foldr (flip (zipWith (flip (:))) . parseBoxLine) (repeat "")

pad :: Int -> T.Text -> T.Text
pad n s = if T.length s >= n then s else T.append s (T.replicate (n - T.length s) " ")

parseLines :: [T.Text] -> (Map.Map Int [Char], [Instruction])
parseLines l =
    case splitOn [""] l of
        [hd, tl] ->
            case splitAt (length hd - 1) hd of
                (boxes, nums) ->
                    let padLength = T.length (head nums) in
                        (parseBoxes (map (pad padLength) boxes), map parseInstruction tl)
        _ -> error "invalid input"

processInstruction :: Map.Map Int [Char] -> Instruction -> Map.Map Int [Char]
processInstruction m [numMove, source, dest] =
    foldr
    (\_ m' ->
        case fromJust $ uncons (m' ! source)
            of (hd, tl) ->
                Map.insert dest (hd : fromJust (Map.lookup dest m')) (Map.insert source tl m'))
    m
    [1..numMove]
processInstruction _ _ = error "invalid instruction"

part1 :: T.Text -> T.Text
part1 s = case parseLines $ T.splitOn "\n" s of
    (m, instr) ->
        T.pack $
        Map.foldr (\s' acc -> head s' : acc) [] $
        foldl' processInstruction m instr

part2 :: T.Text -> T.Text
part2 _ = "bob"
