{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Day15 where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec qualified as Parsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

-- OK so for each beacon we sweep across the given line and mark anything
-- closer than the closest beacon as impossible
-- then we merge the intervals together and see their total size (they may
-- overlap)

type Coord a = (a, a)

manhattanDistance :: (Num a) => Coord a -> Coord a -> a
manhattanDistance (x, y) (a, b) = abs (x - a) + abs (y - b)

-- | manhattanDistance
-- >>> manhattanDistance (1 :: Integer, 3) (6, 7)
-- 9
data Interval a = (Ord a) => Interval a a

deriving instance (Eq a) => Eq (Interval a)

deriving instance (Show a) => Show (Interval a)

instance (Ord a) => Ord (Interval a) where
  compare (Interval x _) (Interval y _) = compare x y

mkInterval :: (Ord a) => a -> a -> Maybe (Interval a)
mkInterval lo hi
  | lo < hi = Just (Interval lo hi)
  | otherwise = Nothing

-- how many times have I coded interval overlap?
-- [lo, hi]  [lo2 hi2] --> lo2 \in lo, hi OR hi2 \in lo, hi
overlaps :: Interval a -> Interval a -> Bool
overlaps (Interval lo hi) (Interval lo2 hi2) =
  not ((hi < lo2) || (lo > hi2))

merge :: Interval a -> Interval a -> Interval a
merge i1 i2 =
  if not (overlaps i1 i2)
    then
      error "intervals must overlap"
    else case (i1, i2) of
      (Interval lo hi, Interval lo2 hi2) -> Interval (min lo lo2) (max hi hi2)

-- OK how I did it for Clojure was find the distance to the line, then form
-- the impossible interval based on what's left over.  this seems reasonable
-- what I'll try to do here is create a tree of these then compact the intervals

-- Void = no error messages
-- Text = type of input stream
type Parser = Parsec Void Text

pLine :: Parser (Coord Integer, Coord Integer)
pLine = do
  _ <- string "Sensor at x="
  x <- L.signed (pure ()) L.decimal
  _ <- string ", y="
  y <- L.signed (pure ()) L.decimal
  _ <- string ": closest beacon is at x="
  cx <- L.signed (pure ()) L.decimal
  _ <- string ", y="
  cy <- L.signed (pure ()) L.decimal
  return ((x, y), (cx, cy))

-- | pLine
-- >>> parseTest pLine "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
-- ((2,18),(-2,15))
-- >>> parseTest pLine "Sensor at x=1115220, y=1782338: closest beacon is at x=1111304, y=1535696"
-- ((1115220,1782338),(1111304,1535696))

-- actual usage:
-- a = Parsec.parse pLine

ruledOutInterval :: (Num a, Ord a) => Coord a -> Coord a -> a -> Maybe (Interval a)
ruledOutInterval (x, y) (bx, by) destY =
  let beaconDistance = manhattanDistance (x, y) (bx, by)
      distToLine = manhattanDistance (x, y) (x, destY)
      diff = beaconDistance - distToLine
   in if
        | diff < 0 -> Nothing
        | otherwise -> Just $ Interval (x - diff) (x + diff)
