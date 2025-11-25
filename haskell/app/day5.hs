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
parseNumLine :: T.Text -> [Char]
parseNumLine = (\s -> map (s !!) [1,5..length s]) . T.unpack

parseBoxLine :: Int -> T.Text -> [Char]
parseBoxLine numLength = (\s -> map (\n -> if n >= length s then ' ' else s !! n) [1,5..numLength]) . T.unpack

parseBoxes :: Int -> Int -> [T.Text] -> Map.Map Int [Char]
parseBoxes numBoxes numLength =
    snd .
    foldr (\s (n, m) -> (n + 1, Map.insert (numBoxes - n + 1) (trim s) m)) (1, Map.empty) .
    foldr (flip (zipWith (flip (:))) . parseBoxLine numLength) (map (const "") [0..numBoxes])

parseLines :: [T.Text] -> (Map.Map Int [Char], [Instruction])
parseLines l =
    case splitOn [""] l of
        [hd, tl] ->
            case splitAt (length hd - 1) hd of
                (boxes, nums) ->
                    let (numBoxes, numLength) = (length (parseNumLine (head nums)), T.length (head nums)) in
                        (parseBoxes numBoxes numLength boxes, map parseInstruction tl)
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
