{-# LANGUAGE MultiWayIf #-}

module Day9 where

import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.List (maximumBy, sort)
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Util (Coord2d, add2, coordRange, parseCoord)

-- | area
-- >>> area (2,5) (9,7)
-- 24
area :: Coord2d -> Coord2d -> Int
area (x, y) (x', y') = (abs (x' - x) + 1) * (abs (y - y') + 1)

part1 :: T.Text -> Int
part1 t =
  maximum $
    uncurry area
      <$> [(coord1, coord2) | coord1 <- coords, coord2 <- coords, coord1 < coord2]
  where
    coords = parseCoord <$> T.splitOn "\n" t

-- my puzzle answer is 4 billionish so attacking this directly will not work
-- we could compress the space.  that may make it tractable.  yes.  that will work.
-- we only have 500ish coords in both x and ys.
-- what is interesting is that the area max calculation then relies on the transformation.
-- OK time to get to it.

-- xCoords :: [Coord2d] -> [Int]
-- xCoords = map fst

-- yCoords :: [Coord2d] -> [Int]
-- yCoords = map snd

type CoordCompression = M.Map Int Int

compressCoord :: (CoordCompression, CoordCompression) -> Coord2d -> Coord2d
compressCoord (xCompress, yCompress) (x, y) = (xCompress ! x, yCompress ! y)

uniqueify :: (Ord a) => [a] -> [a]
uniqueify = Set.toList . foldr Set.insert Set.empty

createCompression :: (Coord2d -> Int) -> [Coord2d] -> CoordCompression
createCompression f coords =
  foldr (\(n, x) m -> M.insert n x m) M.empty $ zip (sort $ uniqueify (f <$> coords)) [0 ..]

-- | uncompress
-- >>> uncompress (M.fromList [(3,1),(4,2)], M.fromList [(2,0)]) (1,0)
-- (3,2)
uncompress :: (CoordCompression, CoordCompression) -> Coord2d -> Coord2d
uncompress (xCompress, yCompress) (x, y) =
  ( fst $ head $ filter (\c -> snd c == x) $ M.toList xCompress,
    fst $ head $ filter (\c -> snd c == y) $ M.toList yCompress
  )

-- final "tricky" thing - determining if a point is in a polygon
-- we can just enumerate all points in our space, shoot a ray from each direction,
-- if it hits all of them it is in the polygon.  we have the walls from the lines.

boundaries :: [Coord2d] -> Set Coord2d
boundaries coords =
  foldr (flip (foldr Set.insert)) Set.empty $ zipWith coordRange coords (tail coords ++ [head coords])

insideRectangle :: Coord2d -> Coord2d -> Coord2d -> Bool
insideRectangle (x, y) (xmin, ymin) (xmax, ymax) =
  xmin <= x && x <= xmax && ymin <= y && y <= ymax

rayHitsEdge :: Coord2d -> Coord2d -> Coord2d -> Set Coord2d -> (Int, Int) -> Bool
rayHitsEdge c lhs rhs edges delta =
  if
    | not $ insideRectangle c lhs rhs -> False
    | c `Set.member` edges -> True
    | otherwise -> rayHitsEdge (add2 c delta) lhs rhs edges delta

-- | polygon
-- >>> s = "7,1\n11,1\n11,7\n9,7\n9,5\n2,5\n2,3\n7,3"
-- >>> polygon $ parseCoord <$> T.splitOn "\n" s
-- fromList [(2,3),(2,4),(2,5),(3,3),(3,4),(3,5),(4,3),(4,4),(4,5),(5,3),(5,4),(5,5),(6,3),(6,4),(6,5),(7,1),(7,2),(7,3),(7,4),(7,5),(8,1),(8,2),(8,3),(8,4),(8,5),(9,1),(9,2),(9,3),(9,4),(9,5),(9,6),(9,7),(10,1),(10,2),(10,3),(10,4),(10,5),(10,6),(10,7),(11,1),(11,2),(11,3),(11,4),(11,5),(11,6),(11,7)]
polygon :: [Coord2d] -> Set Coord2d
polygon coords =
  foldr
    Set.insert
    edges
    $ filter
      ( \coord ->
          all
            (rayHitsEdge coord upperleft lowerright edges)
            [(0, 1), (1, 0), (0, -1), (-1, 0)]
      )
      [(x, y) | x <- [xmin .. xmax], y <- [ymin .. ymax]]
  where
    xmin = minimum $ fst <$> coords
    xmax = maximum $ fst <$> coords
    ymin = minimum $ snd <$> coords
    ymax = maximum $ snd <$> coords
    upperleft = (xmin, ymin)
    lowerright = (xmax, ymax)
    edges = boundaries coords

isValid :: Set Coord2d -> Coord2d -> Coord2d -> Bool
isValid points (x1, y1) (x2, y2) =
  all
    (`Set.member` points)
    [(x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]

uncompressedArea :: (CoordCompression, CoordCompression) -> (Coord2d, Coord2d) -> Int
uncompressedArea compression (c1, c2) =
  area (uncompress compression c1) (uncompress compression c2)

part2 :: T.Text -> Int
part2 t =
  uncurry area
    $ bimap (uncompress compression) (uncompress compression)
    $ maximumBy
      (compare `on` uncompressedArea compression)
    $ filter
      (uncurry (isValid compressedPoints))
      [(coord1, coord2) | coord1 <- compressedCoords, coord2 <- compressedCoords, coord1 < coord2]
  where
    coords = parseCoord <$> T.splitOn "\n" t
    compression = (createCompression fst coords, createCompression snd coords)
    compressedCoords = compressCoord compression <$> coords
    compressedPoints = polygon compressedCoords