{-# LANGUAGE OverloadedStrings #-}

module Day9 where

import qualified Data.Set as S
import qualified Data.Text as T
import Util (Coord2d, Direction (..), add2, delta2d)

parseLine :: T.Text -> (Direction, Int)
parseLine t = case T.splitOn " " t of
  ["L", numStr] -> (West, read $ T.unpack numStr)
  ["R", numStr] -> (East, read $ T.unpack numStr)
  ["D", numStr] -> (South, read $ T.unpack numStr)
  ["U", numStr] -> (North, read $ T.unpack numStr)
  _ -> error "error parsing"

isAdjacent :: Coord2d -> Coord2d -> Bool
isAdjacent (hx, hy) (tx, ty) =
  case (abs $ hx - tx, abs $ hy - ty) of
    (0, 0) -> True
    (1, 0) -> True
    (0, 1) -> True
    (1, 1) -> True
    _ -> False

move :: Direction -> (Coord2d, Coord2d) -> (Coord2d, Coord2d)
move d (h, t) =
  let nextH = add2 (delta2d d) h
   in if isAdjacent t nextH then (nextH, t) else (nextH, h)

expand :: (Direction, Int) -> [Direction]
expand (d, n) = map (const d) [1 .. n]

part1 :: T.Text -> Int
part1 s =
  S.size $
    foldr (S.insert . snd) S.empty $
      scanl (flip move) ((0, 0), (0, 0)) $
        concatMap (expand . parseLine) (T.splitOn "\n" s)

part2 :: T.Text -> Int
part2 _ = 1
