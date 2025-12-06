{-# LANGUAGE OverloadedStrings #-}

module Day11 where

import Data.List (foldl', sortBy, unfoldr)
import Data.Map ((!))
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Parsec (char, newline, optional, spaces, string, try, (<|>))
import qualified Text.Parsec as Parsec

type MonkeyId = Int

data Operation = Mult Int | MultSelf | Add Int deriving (Show)

data Monkey = Monkey
  { monkeyId :: MonkeyId,
    startingItems :: [Int],
    operation :: Operation,
    test :: (Int, MonkeyId, MonkeyId)
  }
  deriving (Show)

number :: Parsec.Parsec T.Text () Int
number = read <$> Parsec.many1 Parsec.digit

parseStartingItems :: Parsec.Parsec T.Text () [Int]
parseStartingItems = do
  _ <- spaces >> Parsec.string "Starting items: "
  n <- number
  rest <- Parsec.many (char ',' >> Parsec.spaces >> number)
  _ <- newline
  return (n : rest)

parseOperation :: Parsec.Parsec T.Text () Operation
parseOperation =
  try
    ( do
        _ <- spaces >> string "Operation: new = old * old"
        return MultSelf
    )
    <|> try
      ( do
          _ <- spaces >> string "Operation: new = old * "
          Mult <$> number
      )
    <|> try
      ( do
          _ <- spaces >> string "Operation: new = old + "
          Add <$> number
      )

parseMonkeyNumber :: Parsec.Parsec T.Text () MonkeyId
parseMonkeyNumber = do
  _ <- string "Monkey "
  n <- number
  _ <- char ':' >> newline
  return n

parseTest :: Parsec.Parsec T.Text () (Int, MonkeyId, MonkeyId)
parseTest = do
  _ <- spaces >> string "Test: divisible by "
  n <- number
  _ <- newline >> spaces >> string "If true: throw to monkey "
  m1 <- number
  _ <- newline >> spaces >> string "If false: throw to monkey "
  m2 <- number
  _ <- optional newline
  return (n, m1, m2)

-- | parseMonkey
-- >>> s = "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3"
-- >>> Parsec.parse parseMonkey "(source)" s
-- Right (Monkey {monkeyId = 0, startingItems = [79,98], operation = Mult 19, test = (23,2,3)})
parseMonkey :: Parsec.Parsec T.Text () Monkey
parseMonkey = do
  n <- parseMonkeyNumber
  items <- parseStartingItems
  op <- parseOperation
  _ <- newline
  t <- parseTest
  return Monkey {monkeyId = n, startingItems = items, operation = op, test = t}

parse :: T.Text -> Monkey
parse s = case Parsec.parse parseMonkey "source" s of
  Right m -> m
  Left e -> error $ "parse error :-( " ++ show e

parseMonkeys :: T.Text -> M.Map MonkeyId Monkey
parseMonkeys =
  foldr
    ((\m -> M.insert (monkeyId m) m) . parse)
    M.empty
    . T.splitOn "\n\n"

monkeyInspect :: Monkey -> Int -> Int
monkeyInspect m k =
  case operation m of
    Mult n -> n * k
    MultSelf -> k * k
    Add n -> n + k

monkeyTest :: Monkey -> Int -> MonkeyId
monkeyTest m k =
  case test m of
    (n, m1, m2) -> if k `mod` n == 0 then m1 else m2

monkeyAddItem :: Int -> Monkey -> Monkey
monkeyAddItem s m = m {startingItems = startingItems m ++ [s]}

monkeyTurn :: MonkeyId -> M.Map MonkeyId Monkey -> (Int, M.Map MonkeyId Monkey)
monkeyTurn mid m =
  ( length items,
    foldl'
      ( \m' s -> do
          let s1 = monkeyInspect monkey s
          let s2 = s1 `div` 3
          M.update (Just . monkeyAddItem s2) (monkeyTest monkey s2) m'
      )
      (M.update (\m' -> Just m' {startingItems = []}) mid m)
      items
  )
  where
    monkey = m ! mid
    items = startingItems monkey

sumMap :: M.Map MonkeyId Int -> M.Map MonkeyId Int -> M.Map MonkeyId Int
sumMap = M.unionWith (+)

monkeyRound :: M.Map MonkeyId Monkey -> (M.Map MonkeyId Int, M.Map MonkeyId Monkey)
monkeyRound m =
  foldl'
    ( \(mInspects, m') mid ->
        let (numInspects, mNext) = monkeyTurn mid m'
         in (M.insert mid numInspects mInspects, mNext)
    )
    (M.empty, m)
    [0 .. numMonkeys]
  where
    numMonkeys = M.size m - 1

rounds :: M.Map MonkeyId Monkey -> [M.Map MonkeyId Int]
rounds = unfoldr (Just . monkeyRound)

part1 :: T.Text -> Int
part1 = product . map snd . take 2 . sortBy (\(_, a) (_, b) -> compare b a) . M.toList . foldr sumMap M.empty . take 20 . rounds . parseMonkeys

part2 :: T.Text -> Int
part2 _ = 1
