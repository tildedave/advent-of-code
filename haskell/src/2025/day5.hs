module Day5 where

import qualified Data.Text as T

parseRange :: T.Text -> (Int, Int)
parseRange t = case read . T.unpack <$> T.splitOn "-" t of
  [lo, hi] -> (lo, hi)
  _ -> error "invalid range"

parseInput :: T.Text -> ([(Int, Int)], [Int])
parseInput t =
  case T.splitOn "\n\n" t of
    [h, tl] -> (parseRange <$> T.splitOn "\n" h, read . T.unpack <$> T.splitOn "\n" tl)
    _ -> error "invalid"

inRange :: Int -> (Int, Int) -> Bool
inRange n (lo, hi) = lo <= n && n <= hi

part1 :: T.Text -> Int
part1 t =
  let (ranges, ingredients) = parseInput t
   in length $ filter (\i -> any (inRange i) ranges) ingredients

-- part2 is simple, we've done stuff like this for AoC before
-- need to have an accumulator where you add the interval to another interval
-- can actually continue to reduce the list until no more overlaps

-- 4 cases
overlaps :: (Int, Int) -> (Int, Int) -> Bool
overlaps (lo, hi) (lo', hi') =
  not (hi < lo' || hi' < lo)

-- | merge
-- >>> merge (16,20) (12,18)
-- (12,20)
merge :: (Int, Int) -> (Int, Int) -> (Int, Int)
merge (lo, hi) (lo', hi') =
  (min lo lo', max hi hi')

-- does it even matter if we merge an interval in more than once to a list?

-- | mergeIn
-- >>> mergeIn (16,20) [(12,18)]
-- [(12,20)]
-- >>> mergeIn (3,5) []
-- [(3,5)]
-- >>> mergeIn (10,14) [(3,5)]
-- [(10,14),(3,5)]
-- >>> mergeIn (16,20) [(10,14),(3,5)]
-- [(16,20),(10,14),(3,5)]
-- >>> mergeIn (12,18) [(16,20),(10,14),(3,5)]
-- [(10,20),(3,5)]
mergeIn :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
mergeIn coord coordList =
  case filter (overlaps coord) coordList of
    h : _ -> mergeIn (merge h coord) [x | x <- coordList, x /= h]
    [] -> coord : coordList

part2 :: T.Text -> Int
part2 =
  sum . fmap ((+ 1) . uncurry subtract) . foldr mergeIn [] . fst . parseInput
