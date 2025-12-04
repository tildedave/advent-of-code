module Day8 where

import Data.Char (digitToInt)
import Data.List (unfoldr)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Util (Coord2d, Direction (..), Grid, add2, cells, delta2d, gridAt, parseGrid)

walk :: Grid Coord2d Int -> Direction -> Coord2d -> [Int]
walk g d =
  drop 1 -- drop the first one
    . unfoldr
      ( \c ->
          case gridAt g c of
            Nothing -> Nothing
            Just x -> Just (x, add2 c (delta2d d))
      )

-- we are visible if we're the largest tree to the top/right/left/North
isVisible :: Grid Coord2d Int -> Coord2d -> Bool
isVisible g c =
  let v = fromJust $ gridAt g c
   in any (\d -> not $ any (>= v) $ walk g d c) [West, East, South, North]

scenicScore :: Grid Coord2d Int -> Coord2d -> Int
scenicScore g c =
  let v = fromJust $ gridAt g c
   in product $
        map
          ( \d ->
              let (seenTrees, restTrees) = span (< v) (walk g d c)
               in length seenTrees + (if null restTrees then 0 else 1)
          )
          [West, East, South, North]

part1 :: T.Text -> Int
part1 t =
  let g = parseGrid digitToInt t
   in length $ filter (isVisible g) $ M.keys (cells g)

part2 :: T.Text -> Int
part2 t =
  let g = parseGrid digitToInt t
   in maximum $ map (scenicScore g) $ M.keys (cells g)
