{-# LANGUAGE ScopedTypeVariables #-}

module Day12 where

import Data.Char (ord)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Search (dijkstraSearch)
import Util (Coord2d, Grid, cardinalNeighbors, gridAt_, gridCoords, gridFind, parseGrid)

elevation :: Char -> Int
elevation 'S' = ord 'a'
elevation 'E' = ord 'z'
elevation ch = ord ch

climbingNeighbors :: Grid Coord2d Char -> Coord2d -> [Coord2d]
climbingNeighbors grid coord =
  filter
    ( \c ->
        let dest = elevation $ gridAt_ c grid
         in (dest - curr <= 1)
    )
    $ cardinalNeighbors grid coord
  where
    curr = elevation $ gridAt_ coord grid

isClimbingGoal :: Grid Coord2d Char -> Coord2d -> Bool
isClimbingGoal grid coord = gridAt_ coord grid == 'E'

-- | Examples
-- >>> s = "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi"
-- >>> part1 s
-- 31
-- >>> part2 s
-- 29
part1 :: T.Text -> Int
part1 t =
  snd $ fromJust $ snd $ dijkstraSearch (gridFind 'S' grid) (climbingNeighbors grid) (isClimbingGoal grid)
  where
    grid = parseGrid id t

reverseClimbingNeighbors :: Grid Coord2d Char -> Coord2d -> [Coord2d]
reverseClimbingNeighbors grid coord =
  filter
    ( \c ->
        let dest = elevation $ gridAt_ c grid
         in (curr - dest <= 1)
    )
    $ cardinalNeighbors grid coord
  where
    curr = elevation $ gridAt_ coord grid

part2 :: T.Text -> Int
part2 t =
  minimum $ map snd $ M.toList $ M.filterWithKey (\k _ -> k `elem` startingPoints) distances
  where
    grid = parseGrid id t
    distances = fst $ dijkstraSearch (gridFind 'E' grid) (reverseClimbingNeighbors grid) (const False)
    startingPoints = filter (\c -> ord 'a' == elevation (gridAt_ c grid)) (gridCoords grid)
