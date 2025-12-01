{-# LANGUAGE OverloadedStrings #-}

module Day1 where

import qualified Data.Text as T
import Data.Text.Read (decimal)
import Data.Either (fromRight)

data Rotation = LeftRotation Int | RightRotation Int

parseRotation :: T.Text -> Rotation
parseRotation s =
    case T.take 1 s of
        "R" -> RightRotation (fst $ fromRight (0, "") $ decimal (T.drop 1 s))
        "L" -> LeftRotation (fst $ fromRight (0, "") $ decimal (T.drop 1 s))
        _ -> error ("invalid: " ++ show s)

-- | applyRotation
-- >>> applyRotation (LeftRotation 68) 50
-- 82
-- >>> applyRotation (LeftRotation 30) 82
-- 52
-- >>> applyRotation (RightRotation 48) 52
-- 0
applyRotation :: Rotation -> Int -> Int
applyRotation (LeftRotation n) dial = (dial - n) `mod` 100
applyRotation (RightRotation n) dial = (dial + n) `mod` 100

dialSequence :: [Rotation] -> [Int]
dialSequence = scanl (flip applyRotation) 50

part1:: T.Text -> Int
part1 = length . filter (== 0) . dialSequence . map parseRotation . T.splitOn "\n"

part2:: T.Text -> Int
part2 _ = 1
