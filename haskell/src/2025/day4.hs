module Day4 where

import Data.List (unfoldr)
import qualified Data.Map as M
import qualified Data.Text as T
import Util (Coord2d, Grid (Grid), bounds, cardinalNeighbors, cells, gridAt', gridCoords, ordinalNeighbors, parseGrid)

neighbors :: Grid Coord2d a -> Coord2d -> [Coord2d]
neighbors g c = cardinalNeighbors g c ++ ordinalNeighbors g c

canRemove :: Grid Coord2d Char -> Coord2d -> Bool
canRemove g c = gridAt' g c == '@' && length (filter (\n -> gridAt' g n == '@') $ neighbors g c) < 4

part1 :: T.Text -> Int
part1 t =
  let g = parseGrid id t
   in length $ filter (canRemove g) (gridCoords g)

-- part2 is just an unfoldr on the previous part

part2 :: T.Text -> Int
part2 t =
  sum $
    unfoldr
      ( \g ->
          let removable = filter (canRemove g) (gridCoords g)
           in if null removable
                then
                  Nothing
                else
                  -- this is pretty ugly, I want to fold this into the Util module
                  let newCells = foldr (`M.insert` '.') (cells g) removable
                   in Just
                        ( length removable,
                          Grid {cells = newCells, bounds = bounds g}
                        )
      )
      (parseGrid id t)
