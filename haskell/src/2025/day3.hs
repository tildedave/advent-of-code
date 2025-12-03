module Day3 where

import Data.Char (digitToInt)
import Data.Foldable (maximumBy)
import Data.Function (on)
import qualified Data.Text as T

-- | highestVoltage
-- >>> highestVoltage [9,8,7,6,5,4,3,2,1,1,1,1,1,1,1]
-- 98
highestVoltage :: [Int] -> Int
highestVoltage l =
  let h = maximum (init l)
   in let rest = dropWhile (< h) l
       in h * 10 + maximum (drop 1 rest)

part1 :: T.Text -> Int
part1 = sum . map (highestVoltage . map digitToInt . T.unpack) . T.splitOn "\n"

part2 :: T.Text -> Int
part2 _ = 1
