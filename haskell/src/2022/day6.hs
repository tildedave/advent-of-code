module Day6 where

import Data.List (findIndex)
import Data.Maybe (fromJust)
import qualified Data.Text as T

allUnique :: (Eq a) => [a] -> Bool
allUnique [] = True
allUnique (x : xs) = (x `notElem` xs) && allUnique xs

sections :: Int -> T.Text -> [T.Text]
sections n s = filter (\s' -> T.length s' == n) $ T.transpose $ map (`T.drop` s) [0 .. n - 1]

-- | part1
-- >>> part1 (T.pack "bvwbjplbgvbhsrlpgdmjqwftvncz")
-- 5
markerPosition :: Int -> T.Text -> Int
markerPosition n =
  (+) n
    . fromJust
    . findIndex (\s' -> allUnique (map (T.index s') [0 .. n - 1]))
    . sections n

part1 :: T.Text -> Int
part1 = markerPosition 4

-- | part2
-- >>> part2 (T.pack "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
-- 19
part2 :: T.Text -> Int
part2 = markerPosition 14
