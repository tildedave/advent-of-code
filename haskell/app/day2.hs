{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import qualified Data.Text as T

data Choice = Rock | Paper | Scissors deriving (Show, Eq)
data Result = Win | Lose | Draw deriving (Show)

parseChoice :: Char -> Choice
parseChoice 'A' = Rock
parseChoice 'B' = Paper
parseChoice 'C' = Scissors
parseChoice 'X' = Rock
parseChoice 'Y' = Paper
parseChoice 'Z' = Scissors
parseChoice _ = error "invalid input"

shapeScore :: Choice -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

result :: (Choice, Choice) -> Result
result (Paper, Rock) = Win
result (Scissors, Paper) = Win
result (Rock, Scissors) = Win
result (Paper, Paper) = Draw
result (Rock, Rock) = Draw
result (Scissors, Scissors) = Draw
result (_, _) = Lose

resultScore :: Result -> Int
resultScore Win = 6
resultScore Draw = 3
resultScore Lose = 0

score :: (Choice, Choice) -> Int
score c = resultScore (result c) + shapeScore (fst c)

parseGame :: T.Text -> (Choice, Choice)
parseGame s = (parseChoice $ T.last s, parseChoice $ T.head s)

-- | Foo
-- >>> lineScore (T.pack "A Y")
-- 8
-- >>> lineScore (T.pack "B X")
-- 1
-- >>> lineScore (T.pack "C Z")
-- 6
lineScore :: T.Text -> Int
lineScore = score . parseGame

part1 :: T.Text -> Int
part1 = sum . map lineScore . T.splitOn "\n"

parseResult :: Char -> Result
parseResult 'X' = Lose
parseResult 'Y' = Draw
parseResult 'Z' = Win
parseResult _ = error "Invalid result"

move :: Choice -> Result -> Choice
move Paper Win = Scissors
move Paper Lose = Rock
move Scissors Win = Rock
move Scissors Lose = Paper
move Rock Win = Paper
move Rock Lose = Scissors
move m Draw = m

parseStrategy :: T.Text -> (Choice, Result)
-- feels like there is a better way to do this
-- "point-free" way per https://pointfree.io/ is this:
-- parseStrategy = ap ((,) . parseChoice . T.head) (parseResult . T.last)
parseStrategy s = (parseChoice $ T.head s, parseResult $ T.last s)

lineScorePart2 :: (Choice, Result) -> Int
lineScorePart2 (opponentMove, r) = score (move opponentMove r, opponentMove)

part2 :: T.Text -> Int
part2 = sum . map (lineScorePart2 . parseStrategy). T.splitOn "\n"
