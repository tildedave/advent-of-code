{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Day1 where

import qualified Text.Parsec as Parsec
import qualified Data.Text as T
import Data.List (foldl')

data Rotation = LeftRotation Int | RightRotation Int deriving Show

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
applyRotation dial (LeftRotation n) = dial - n
applyRotation dial (RightRotation n) = dial + n

dialSequence :: [Rotation] -> [Int]
dialSequence = scanl applyRotation 50

part1:: T.Text -> Int
part1 = length . filter (\n -> 0 == n `mod` 100) . dialSequence . map parseRotation . T.splitOn "\n"

-- | password0x434C49434B
-- >>> password0x434C49434B (50, 0) (LeftRotation 68)
-- (82,1)
-- >>> password0x434C49434B (82, 1) (LeftRotation 30)
-- (52,1)
-- >>> password0x434C49434B (52, 1) (RightRotation 48)
-- (0,2)
-- >>> password0x434C49434B (0, 2) (LeftRotation 5)
-- (95,2)
password0x434C49434B :: (Int, Int) -> Rotation -> (Int, Int)
password0x434C49434B (prev, count) (LeftRotation n) =
    let next = prev - n in
        (next `mod` 100, count + length (takeWhile (>= next) $ drop (if prev == 0 then 1 else 0) [0,-100..] ))
password0x434C49434B (prev, count) (RightRotation n) =
    let next = prev + n in
        (next `mod` 100, count + length (takeWhile (<= next) [100,200..] ))

part2:: T.Text -> Int
part2 =
    snd .
    foldl' password0x434C49434B (50, 0) .
    map parseRotation .
    T.splitOn "\n"
