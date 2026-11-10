{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Day15 where

import Data.List
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
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
type Parser = Parsec Void T.Text

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

mergeIntervalIn :: (Ord a) => List (Interval a) -> (Interval a) -> List (Interval a)
mergeIntervalIn [] i = [i]
mergeIntervalIn (x : xs) i =
  if overlaps x i then mergeIntervalIn xs (merge x i) else x : (mergeIntervalIn xs i)

lineToImpossibilityInterval :: Integer -> T.Text -> Maybe (Interval Integer)
lineToImpossibilityInterval y t =
  case Parsec.parse pLine "source" t of
    Right (c1, c2) -> ruledOutInterval c1 c2 y
    Left e -> error $ "parse error :-( " ++ show e

-- | lineToImpossibilityInterval
-- >>> lineToImpossibilityInterval 10 "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
-- Nothing
distance :: (Num p) => Interval p -> p
distance (Interval x y) = y - x

impossibilityDistance :: Integer -> T.Text -> Integer
impossibilityDistance y t =
  let intervals = mapMaybe (lineToImpossibilityInterval y) (T.splitOn "\n" t)
   in sum $
        map distance $
          foldl
            mergeIntervalIn
            []
            intervals

-- | impossibilityDistance
-- >>> impossibilityDistance 10 "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\nSensor at x=9, y=16: closest beacon is at x=10, y=16\nSensor at x=13, y=2: closest beacon is at x=15, y=3\nSensor at x=12, y=14: closest beacon is at x=10, y=16\nSensor at x=10, y=20: closest beacon is at x=10, y=16\nSensor at x=14, y=17: closest beacon is at x=10, y=16\nSensor at x=8, y=7: closest beacon is at x=2, y=10\nSensor at x=2, y=0: closest beacon is at x=2, y=10\nSensor at x=0, y=11: closest beacon is at x=2, y=10\nSensor at x=20, y=14: closest beacon is at x=25, y=17\nSensor at x=17, y=20: closest beacon is at x=21, y=22\nSensor at x=16, y=7: closest beacon is at x=15, y=3\nSensor at x=14, y=3: closest beacon is at x=15, y=3\nSensor at x=20, y=1: closest beacon is at x=15, y=3"
-- 26
part1 :: T.Text -> Integer
part1 t = impossibilityDistance 2000000 (T.stripEnd t)

-- part 2 can be brute forced, since 4000000 isn't a crazy loop.  (what I did
-- for Clojure). just run our impossibility distance for every y.  but:
-- we know the answer is the only place in the range that isn't covered
-- by a sensor.  therefore the answer must be +1 the distance to the nearest
-- beacon for some number of beacons.  so we can get a list of candidates for
-- each beacon pair, then eliminate the candidates.

lineToDistance :: T.Text -> (Coord Integer, Integer)
lineToDistance t =
  case Parsec.parse pLine "source" t of
    Right (c1, c2) -> (c1, manhattanDistance c1 c2)
    Left e -> error $ "parse error :-( " ++ show e

-- so these are the pairs to check
-- not clear this works,
-- example: ((2,18),(14,17))  x=14, y=11 --> distance 6 from 14,17 (actual is 5)
-- so the pairs don't work, but it is around the 14,17 perimeter
-- we can just check the perimeters of every beacon to see if they're the
-- answer.  that will work.

perimeter :: Coord Integer -> (Coord Integer, Integer) -> [Coord Integer]
perimeter (xmax, ymax) ((x, y), d) =
  [(x + dx, y + dy) | dx <- [-d - 1 .. d + 1], dy <- [-d - 1 .. d + 1], abs (dx + dy) == d + 1, x + dx >= 0, x + dx <= xmax, y + dy >= 0, y + dy <= ymax]

isForbidden :: (Ord a, Num a) => Coord a -> (Coord a, a) -> Bool
isForbidden beacon (sensor, d) = manhattanDistance beacon sensor <= d

-- so the possibilities are the perimeter of each one
-- we combine them all into a set (sure?)

checksum :: Coord Integer -> Integer
checksum (x, y) = x * 4000000 + y

_part2 :: (Integer, Integer) -> T.Text -> Integer
_part2 (xmax, ymax) t =
  let coordsWithDistances = map lineToDistance (T.splitOn "\n" (T.strip t))
      candidates = foldl (\s x -> foldl (flip S.insert) s (perimeter (xmax, ymax) x)) S.empty coordsWithDistances
   in checksum $ S.findMin $ S.filter (\c -> all (\x -> not (isForbidden c x)) coordsWithDistances) candidates

part2 :: T.Text -> Integer
part2 = _part2 (4000000, 4000000)

-- | _part2
-- >>> _part2 (20, 20) "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\nSensor at x=9, y=16: closest beacon is at x=10, y=16\nSensor at x=13, y=2: closest beacon is at x=15, y=3\nSensor at x=12, y=14: closest beacon is at x=10, y=16\nSensor at x=10, y=20: closest beacon is at x=10, y=16\nSensor at x=14, y=17: closest beacon is at x=10, y=16\nSensor at x=8, y=7: closest beacon is at x=2, y=10\nSensor at x=2, y=0: closest beacon is at x=2, y=10\nSensor at x=0, y=11: closest beacon is at x=2, y=10\nSensor at x=20, y=14: closest beacon is at x=25, y=17\nSensor at x=17, y=20: closest beacon is at x=21, y=22\nSensor at x=16, y=7: closest beacon is at x=15, y=3\nSensor at x=14, y=3: closest beacon is at x=15, y=3\nSensor at x=20, y=1: closest beacon is at x=15, y=3"
-- 56000011
