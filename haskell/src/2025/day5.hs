module Day5 where

import qualified Data.Text as T

parseRange :: T.Text -> (Int, Int)
parseRange t = case map (read . T.unpack) $ T.splitOn "-" t of
  [lo, hi] -> (lo, hi)
  _ -> error "invalid range"

parseInput :: T.Text -> ([(Int, Int)], [Int])
parseInput t =
  case T.splitOn "\n\n" t of
    [h, tl] -> (map parseRange $ T.splitOn "\n" h, map (read . T.unpack) (T.splitOn "\n" tl))
    _ -> error "invalid"

inRange :: Int -> (Int, Int) -> Bool
inRange n (lo, hi) = lo <= n && n <= hi

part1 :: T.Text -> Int
part1 t =
  let (ranges, ingredients) = parseInput t
   in length $ filter (\i -> any (inRange i) ranges) ingredients

part2 :: T.Text -> Int
part2 _ = 1
