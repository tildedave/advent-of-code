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

compareInt :: (Ord a) => a -> a -> Int
compareInt x y = case compare x y of
  LT -> -1
  GT -> 1
  EQ -> 0

tailStep :: Coord2d -> Coord2d -> Coord2d
tailStep (hx, hy) (tx, ty) =
  let (dtx, dty) = (compareInt hx tx, compareInt hy ty)
   in (tx + dtx, ty + dty)

follow :: Coord2d -> [Coord2d] -> [Coord2d]
follow _ [] = []
follow next (x : xs) =
  let xnext = (if isAdjacent next x then x else tailStep next x) in xnext : follow xnext xs

move :: [Coord2d] -> Direction -> [Coord2d]
move (h : tails) d =
  -- for each item in tails list, if it's no longer adjacent to the p, move to the previous spot
  let nextH = add2 (delta2d d) h
   in nextH : follow nextH tails
move _ _ = error "invalid"

expand :: (Direction, Int) -> [Direction]
expand (d, n) = map (const d) [1 .. n]

part1 :: T.Text -> Int
part1 s =
  S.size $
    foldr (S.insert . last) S.empty $
      scanl move (replicate 2 (0, 0)) $
        concatMap (expand . parseLine) (T.splitOn "\n" s)

part2 :: T.Text -> Int
part2 s =
  S.size $
    foldr (S.insert . last) S.empty $
      scanl move (replicate 10 (0, 0)) $
        concatMap (expand . parseLine) (T.splitOn "\n" s)
