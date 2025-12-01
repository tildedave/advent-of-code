{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Day1 where

import qualified Text.Parsec as Parsec
import qualified Data.Text as T

data Rotation = LeftRotation Int | RightRotation Int deriving Show

-- obviously don't need Parsec here but want to have an example in my codebase
-- for future problems
rotationParser :: Parsec.Parsec T.Text () Rotation
rotationParser = do
    direction <- Parsec.oneOf "RL"
    number <- Parsec.many1 Parsec.digit
    return (case direction of
        'R' -> RightRotation (read number)
        'L' -> LeftRotation (read number)
        _ -> error "impossible")

parseRotation :: T.Text -> Rotation
parseRotation s =
    case Parsec.parse rotationParser "(source)" s of
        Right r -> r
        _ -> error "invalid"

-- | applyRotation
-- >>> applyRotation 50 (LeftRotation 68)
-- -18
-- >>> applyRotation 82 (LeftRotation 30)
-- 52
-- >>> applyRotation 52 (RightRotation 48)
-- 100
applyRotation :: Int -> Rotation -> Int
applyRotation dial (LeftRotation n) = (dial - n) `mod` 100
applyRotation dial (RightRotation n) = (dial + n) `mod` 100

dialSequence :: [Rotation] -> [Int]
dialSequence = scanl applyRotation 50

part1:: T.Text -> Int
part1 = length . filter (\n -> 0 == n `mod` 100) . dialSequence . map parseRotation . T.splitOn "\n"

-- | numTurns
-- >>> numTurns 50 (LeftRotation 68)
-- 1
-- >>> numTurns 82 (LeftRotation 30)
-- 0
-- >>> numTurns 52 (RightRotation 48)
-- 1
-- >>> numTurns 0 (LeftRotation 5)
-- 0
numTurns :: Int -> Rotation -> Int
numTurns prev (LeftRotation n) =
    let next = prev - n in
        length (takeWhile (>= next) $ drop (if prev == 0 then 1 else 0) [0,-100..])

numTurns prev (RightRotation n) =
    let next = prev + n in
        length (takeWhile (<= next) [100,200..])

part2:: T.Text -> Int
part2 l =
    let rots = map parseRotation $ T.splitOn "\n" l in
        sum $ zipWith numTurns (dialSequence rots) rots
