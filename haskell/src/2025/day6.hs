{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import Data.Char (digitToInt)
import Data.Function (on)
import Data.List (foldl1', maximumBy, transpose)
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Text as T
import Util (trim, unsnoc)

applyOperation :: [T.Text] -> Int
applyOperation l =
  let (ops, op) = fromJust $ unsnoc l
   in ( case op of
          "+" -> sum
          "*" -> product
          _ -> error "invalid"
      )
        $ (read . T.unpack <$> ops)

part1 :: T.Text -> Int
part1 t =
  sum $ map applyOperation $ transpose $ filter (not . T.null) . T.split (== ' ') <$> T.splitOn "\n" t

-- | headerIndices
-- >>> headerIndices "*   +   *   +  " 15
-- [(0,4),(4,8),(8,12),(12,15)]
headerIndices :: T.Text -> Int -> [(Int, Int)]
headerIndices t len =
  let l = map fst $ filter ((/= ' ') . snd) $ zip [0, 1 ..] (T.unpack t)
   in zip l (tail l ++ [len])

-- | digits
-- >>> digits (4,8) "123 328  51 64 "
-- [Just 3,Just 2,Just 8,Nothing]
-- >>> digits (4,8) " 45 64  387 23 "
-- [Just 6,Just 4,Nothing,Nothing]
-- >>> digits (8,12) "123 328  51 64 "
-- [Nothing,Just 5,Just 1,Nothing]
-- >>> map (digits (0,4)) ["123 328  51 64"," 45 64  387 23","  6 98  215 314"]
-- [[Just 1,Just 2,Just 3,Nothing],[Nothing,Just 4,Just 5,Nothing],[Nothing,Nothing,Just 6,Nothing]]
-- >>> map (digits (12,15)) ["123 328  51 64 "," 45 64  387 23 ","  6 98  215 314"]
-- [[Just 6,Just 4,Nothing],[Just 2,Just 3,Nothing],[Just 3,Just 1,Just 4]]
digits :: (Int, Int) -> T.Text -> [Maybe Int]
digits (start, end) t =
  ( \case
      ' ' -> Nothing
      d -> Just $ digitToInt d
  )
    . T.index t
    <$> [start .. end - 1]

-- | combineDigits
-- >>> combineDigits [Nothing,Nothing,Nothing,Nothing] [Just 3,Just 2,Just 8,Nothing]
-- [Just 3,Just 2,Just 8,Nothing]
-- >>> combineDigits [Just 3,Just 2,Just 8,Nothing] [Just 6,Just 4,Nothing,Nothing]
-- [Just 36,Just 24,Just 8,Nothing]
-- >>> foldl1' combineDigits [[Just 1,Just 2,Just 3],[Nothing,Just 4,Just 5],[Nothing,Nothing,Just 6]]
-- [Just 1,Just 24,Just 356]
combineDigits :: [Maybe Int] -> [Maybe Int] -> [Maybe Int]
combineDigits =
  zipWith
    ( curry
        ( \case
            (Nothing, Nothing) -> Nothing
            (Nothing, m2) -> m2
            (m1, Nothing) -> m1
            (Just d1, Just d2) -> Just $ (d1 * 10) + d2
        )
    )

-- | parseOperation
-- >>> parseOperation (8,12) "*   +   *   +  "
-- '*'
parseOperation :: (Int, Int) -> T.Text -> Char
parseOperation indices = head . trim . take (snd indices - fst indices) . drop (fst indices) . T.unpack

part2 :: T.Text -> Int
part2 t =
  let (l, ops) = fromJust $ unsnoc $ T.splitOn "\n" t
   in sum $
        map
          ( \indices ->
              let result = catMaybes $ foldl1' combineDigits $ map (digits indices) l
               in case parseOperation indices ops of
                    '+' -> sum result
                    '*' -> product result
                    _ -> error "invalid"
          )
          (headerIndices ops (T.length (maximumBy (compare `on` T.length) l)))
