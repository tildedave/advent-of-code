module Day15 where

-- OK so for each beacon we sweep across the given line and mark anything
-- closer than the closest beacon as impossible
-- then we merge the intervals together and see their total size (they may
-- overlap)

manhattanDistance :: (Num a) => (a, a) -> (a, a) -> a
manhattanDistance (x, y) (a, b) = abs (x - a) + abs (y - b)

data Interval = Interval Int Int deriving (Eq, Show)

mkInterval :: Int -> Int -> Maybe Interval
mkInterval lo hi
  | lo < hi = Just (Interval lo hi)
  | otherwise = Nothing

-- how many times have I coded interval overlap?
-- [lo, hi]  [lo2 hi2] --> lo2 \in lo, hi OR hi2 \in lo, hi
overlaps :: Interval -> Interval -> Bool
overlaps (Interval lo hi) (Interval lo2 hi2) =
  not ((hi < lo2) || (lo > hi2))

merge :: Interval -> Interval -> Interval
merge i1 i2 =
  if not (overlaps i1 i2)
    then
      error "intervals must overlap"
    else case (i1, i2) of
      (Interval lo hi, Interval lo2 hi2) -> Interval (min lo lo2) (max hi hi2)

-- OK how I did it for Clojure was find the distance to the line, then form
-- the impossible interval based on what's left over.  this seems reasonable
