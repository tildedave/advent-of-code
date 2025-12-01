module Day8 where

import Data.Char (digitToInt)
import Data.List (unfoldr)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Util (Coord2d, Grid, cells, gridAt, parseGrid)

data Direction = Left' | Right' | Down | Up deriving (Show)

delta2d :: Direction -> (Int, Int)
delta2d Left' = (-1, 0)
delta2d Right' = (1, 0)
delta2d Down = (0, 1)
delta2d Up = (0, -1)

add2 :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
add2 (x, y) (dx, dy) = (x + dx, y + dy)

walk :: Grid Coord2d Int -> Direction -> Coord2d -> [Int]
walk g d =
  drop 1 -- drop the first one
    . unfoldr
      ( \c ->
          case gridAt g c of
            Nothing -> Nothing
            Just x -> Just (x, add2 c (delta2d d))
      )

-- we are visible if we're the largest tree to the top/right/left/down
isVisible :: Grid Coord2d Int -> Coord2d -> Bool
isVisible g c =
  let v = fromJust $ gridAt g c
   in any (\d -> not $ any (>= v) $ walk g d c) [Left', Right', Up, Down]

scenicScore :: Grid Coord2d Int -> Coord2d -> Int
scenicScore g c =
  let v = fromJust $ gridAt g c
   in product $
        map
          ( \d ->
              let (seenTrees, restTrees) = span (< v) (walk g d c)
               in length seenTrees + (if null restTrees then 0 else 1)
          )
          [Left', Right', Up, Down]

part1 :: T.Text -> Int
part1 l =
  let g = parseGrid digitToInt $ T.splitOn "\n" l
   in length $ filter (isVisible g) $ M.keys (cells g)

part2 :: T.Text -> Int
part2 l =
  let g = parseGrid digitToInt $ T.splitOn "\n" l
   in maximum $ map (scenicScore g) $ M.keys (cells g)
