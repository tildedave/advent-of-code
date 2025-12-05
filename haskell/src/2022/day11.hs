{-# LANGUAGE OverloadedStrings #-}

module Day11 where

import qualified Data.Text as T
import Text.Parsec (char, newline, optional, spaces, string, (<|>))
import qualified Text.Parsec as Parsec

type MonkeyId = Int

data Operation = Mult Int | Add Int deriving (Show)

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
parseOperation = do
  _ <- spaces >> string "Operation: new = old "
  op <- char '*' <|> char '+'
  _ <- spaces
  d <- number
  _ <- newline
  return $ case op of
    '*' -> Mult d
    '+' -> Add d
    _ -> error "inmpossible"

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
  t <- parseTest
  return Monkey {monkeyId = n, startingItems = items, operation = op, test = t}
