module Day2 where

import Data.List.Split (chunksOf)
import qualified Data.Text as T

-- hasDuplicate :: (Eq a) => [a] -> Bool
-- hasDuplicate [] = False
-- hasDuplicate [_] = False
-- hasDuplicate (x : y : xs) = (x == y) || hasDuplicate (y : xs)

-- hasRepeatedDigit :: Int -> Bool
-- hasRepeatedDigit n = hasDuplicate (show n)

-- | isInvalidId
-- >>> isInvalidId 22
-- True
-- >>> isInvalidId 23
-- False
-- >>> isInvalidId 1188511885
-- True
isInvalidId :: Int -> Bool
isInvalidId n = uncurry (==) $ splitAt (length l `div` 2) l where l = show n

-- | invalidIdSum
-- >>> invalidIdSum 1698522 1698528
-- 0
-- >>> invalidIdSum 38593856 38593862
-- 38593859
invalidIdSum :: (Int -> Bool) -> Int -> Int -> Int
invalidIdSum checker l h = sum $ filter checker [l .. h]

-- | parseRange
-- >>> parseRange $ T.pack "1698522-1698528"
-- (1698522,1698528)
parseRange :: T.Text -> (Int, Int)
parseRange s = case T.splitOn "-" s of
  [lStr, hStr] -> (read $ T.unpack lStr, read $ T.unpack hStr)
  _ -> error ("parse error: " ++ show s)

part1 :: T.Text -> Int
part1 = sum . map (uncurry (invalidIdSum isInvalidId) . parseRange) . T.splitOn ","

allSame :: (Eq a) => [a] -> Bool
allSame [] = True
allSame [_] = True
allSame (x : y : l) = x == y && allSame (y : l)

-- | isInvalidIdPart2
-- >>> isInvalidIdPart2 22
-- True
-- >>> isInvalidIdPart2 123123123
-- True
-- >>> isInvalidIdPart2 2121212121
-- True
isInvalidIdPart2 :: Int -> Bool
isInvalidIdPart2 n =
  let l = show n
   in any
        (\k -> allSame $ chunksOf (length l `div` k) l)
        (filter (\k -> length l `mod` k == 0) [2 .. length l])

-- allSame $ chunksOf (length l `div` 2) l where l = show n

part2 :: T.Text -> Int
part2 = sum . map (uncurry (invalidIdSum isInvalidIdPart2) . parseRange) . T.splitOn ","
