{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util where

import Data.Char (isSpace)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Text as T

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

-- | pad
-- >>> pad 10 "boo"
-- "boo       "
-- >>> pad 3 "boom"
-- "boom"
pad :: Int -> String -> String
pad n s = if length s >= n then s else s ++ replicate (n - length s) ' '

-- | modPositive
-- >>> modPositive (-5) 6
-- 1
-- >>> modPositive 13 7
-- 6
modPositive :: Int -> Int -> Int
modPositive n m =
  let r = n `mod` m
   in if r < 0 then modPositive (r + m) m else r

class Neighbors a where
  neighbors :: a -> [a]

type Coord2d = (Int, Int)

data Grid k a = Grid
  { cells :: M.Map k a,
    bounds :: k
  }
  deriving (Show, Foldable)

gridCoords :: Grid k a -> [k]
gridCoords g = M.keys (cells g)

parseGridContents :: (Char -> a) -> [T.Text] -> M.Map Coord2d a
parseGridContents f =
  foldr
    ( \(y :: Int, t) acc ->
        foldr
          (\(x :: Int, c) -> M.insert (x, y) c)
          acc
          (zip [0 ..] (map f (T.unpack t)))
    )
    M.empty
    . zip [0 ..]

-- unexpected: maximum works for tuples
-- Note - bounds are inclusive, <= maxx etc
parseGrid :: (Char -> a) -> T.Text -> Grid Coord2d a
parseGrid f l =
  let cells_ = parseGridContents f $ T.splitOn "\n" l
   in Grid {cells = cells_, bounds = maximum (M.keys cells_)}

cardinalNeighbors :: Grid Coord2d a -> Coord2d -> [Coord2d]
cardinalNeighbors g (x, y) =
  filter
    (`M.member` cells g)
    [ (x + 1, y),
      (x - 1, y),
      (x, y + 1),
      (x, y - 1)
    ]

ordinalNeighbors :: Grid Coord2d a -> Coord2d -> [Coord2d]
ordinalNeighbors g (x, y) =
  filter
    (`M.member` cells g)
    [ (x + 1, y + 1),
      (x + 1, y - 1),
      (x - 1, y + 1),
      (x - 1, y - 1)
    ]

gridAt :: (Ord k) => Grid k a -> k -> Maybe a
gridAt g k = M.lookup k (cells g)

gridAt' :: (Ord k) => Grid k a -> k -> a
gridAt' g k = fromJust $ M.lookup k (cells g)

data Direction = West | East | North | South deriving (Show, Eq)

delta2d :: Direction -> (Int, Int)
delta2d West = (-1, 0)
delta2d East = (1, 0)
delta2d North = (0, 1)
delta2d South = (0, -1)

add2 :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
add2 (x, y) (dx, dy) = (x + dx, y + dy)

unsnoc :: [b] -> Maybe ([b], b)
unsnoc [] = Nothing
unsnoc l = Just (init l, last l)
