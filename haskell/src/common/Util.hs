{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util where

import Data.Char (isSpace)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Text qualified as T

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

type Grid k a = M.Map k a

gridCoords :: Grid k a -> [k]
gridCoords = M.keys

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
bounds :: Grid Coord2d a -> Coord2d
bounds = maximum . M.keys

parseGrid :: (Char -> a) -> T.Text -> Grid Coord2d a
parseGrid f l = parseGridContents f $ T.splitOn "\n" l

cardinalNeighbors :: Grid Coord2d a -> Coord2d -> [Coord2d]
cardinalNeighbors g (x, y) =
  filter
    (`M.member` g)
    [ (x + 1, y),
      (x - 1, y),
      (x, y + 1),
      (x, y - 1)
    ]

ordinalNeighbors :: Grid Coord2d a -> Coord2d -> [Coord2d]
ordinalNeighbors g (x, y) =
  filter
    (`M.member` g)
    [ (x + 1, y + 1),
      (x + 1, y - 1),
      (x - 1, y + 1),
      (x - 1, y - 1)
    ]

gridAt :: (Ord k) => k -> Grid k a -> Maybe a
gridAt = M.lookup

gridAt' :: (Ord k) => k -> Grid k a -> a
gridAt' k g = fromJust $ M.lookup k g

gridFind :: (Ord k, Eq a) => a -> Grid k a -> k
gridFind v m = fst $ M.findMin $ M.filter (== v) m

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
