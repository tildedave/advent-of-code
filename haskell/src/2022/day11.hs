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

class Monkeyable a where
  monkeyTest :: a -> Int -> Bool
  monkeyMult :: a -> Int -> a
  monkeyPlus :: a -> Int -> a
  monkeySquare :: a -> a
  monkeyReduceWorryLevel :: a -> a

instance Monkeyable Int where
  monkeyTest n k = n `mod` k == 0
  monkeyMult n k = n * k
  monkeyPlus n k = n + k
  monkeySquare n = n * n
  monkeyReduceWorryLevel n = n `div` 3

data Monkey = Monkey
  { monkeyId :: MonkeyId,
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
-- Right ([79,98],Monkey {monkeyId = 0, operation = Mult 19, test = (23,2,3)})
parseMonkey :: Parsec.Parsec T.Text () ([Int], Monkey)
parseMonkey = do
  n <- parseMonkeyNumber
  items <- parseStartingItems
  op <- parseOperation
  _ <- newline
  t <- parseTest
  return (items, Monkey {monkeyId = n, operation = op, test = t})

parse :: T.Text -> ([Int], Monkey)
parse s = case Parsec.parse parseMonkey "source" s of
  Right m -> m
  Left e -> error $ "parse error :-( " ++ show e

parseMonkeys :: T.Text -> (M.Map MonkeyId Monkey, M.Map MonkeyId [Int])
parseMonkeys t =
  foldr
    ( \t' (monkeys, items) ->
        let (monkeyItems, monkey) = parse t'
         in ( M.insert (monkeyId monkey) monkey monkeys,
              M.insert (monkeyId monkey) monkeyItems items
            )
    )
    (M.empty, M.empty)
    $ T.splitOn "\n\n" t

monkeyInspect :: (Monkeyable a) => Monkey -> a -> a
monkeyInspect m k =
  case operation m of
    Mult n -> monkeyMult k n
    MultSelf -> monkeySquare k
    Add n -> monkeyPlus k n

nextMonkey :: (Monkeyable a) => Monkey -> a -> MonkeyId
nextMonkey m n =
  case test m of
    (k, m1, m2) -> if monkeyTest n k then m1 else m2

monkeyTurn :: (Monkeyable a) => Monkey -> M.Map MonkeyId [a] -> (Int, M.Map MonkeyId [a])
monkeyTurn monkey mItems =
  ( length items,
    foldl'
      ( \m' s -> do
          let s1 = monkeyInspect monkey s
          let s2 = monkeyReduceWorryLevel s1
          M.update (\l -> Just $ l ++ [s2]) (nextMonkey monkey s2) m'
      )
      (M.insert mid [] mItems)
      (mItems ! mid)
  )
  where
    mid = monkeyId monkey
    items = mItems ! mid

sumMap :: M.Map MonkeyId Int -> M.Map MonkeyId Int -> M.Map MonkeyId Int
sumMap = M.unionWith (+)

monkeyRound :: (Monkeyable a) => M.Map MonkeyId Monkey -> M.Map MonkeyId [a] -> (M.Map MonkeyId Int, M.Map MonkeyId [a])
monkeyRound monkeys mItems =
  foldl'
    ( \(mInspects, m') mid ->
        let (numInspects, mNext) = monkeyTurn (monkeys ! mid) m'
         in (M.insert mid numInspects mInspects, mNext)
    )
    (M.empty, mItems)
    [0 .. numMonkeys]
  where
    numMonkeys = M.size monkeys - 1

rounds :: (Monkeyable a) => M.Map MonkeyId Monkey -> M.Map MonkeyId [a] -> [M.Map MonkeyId Int]
rounds monkeys = unfoldr (Just . monkeyRound monkeys)

-- | monkeyBusiness
-- >>> s = "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1"
-- >>> foldr sumMap M.empty $ take 20 $ uncurry rounds $ parseMonkeys s
-- fromList [(0,101),(1,95),(2,7),(3,105)]
-- >>> foldr sumMap M.empty $ take 1000 $ uncurry rounds $ parseMonkeys s
-- fromList [(0,5204),(1,4792),(2,199),(3,5192)]
monkeyBusiness :: Int -> T.Text -> Int
monkeyBusiness n =
  product
    . map snd
    . take 2
    . sortBy (\(_, a) (_, b) -> compare b a)
    . M.toList
    . foldr sumMap M.empty
    . take n
    . uncurry rounds
    . parseMonkeys

part1 :: T.Text -> Int
part1 = monkeyBusiness 20

part2 :: T.Text -> Int
part2 = monkeyBusiness 10000
