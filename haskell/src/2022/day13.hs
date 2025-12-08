{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day13 where

import Data.List (sort)
import Data.List.Split (chunksOf)
import Data.Text qualified as T
import Text.Parsec ((<|>))
import Text.Parsec qualified as Parsec

data Packet = PacketList [Packet] | PacketNumber Int deriving (Show, Eq)

instance Ord Packet where
  compare (PacketNumber n) (PacketNumber m) = compare n m
  compare (PacketList (x : xs)) (PacketList (y : ys)) =
    case compare x y of
      EQ -> compare xs ys
      o -> o
  compare (PacketList []) (PacketList []) = EQ
  compare (PacketList []) (PacketList _) = LT
  compare (PacketList _) (PacketList []) = GT
  compare (PacketNumber n) p = compare (PacketList [PacketNumber n]) p
  compare p (PacketNumber n) = compare p (PacketList [PacketNumber n])

-- I obviously am still learning Parsec
packetList :: Parsec.Parsec T.Text () Packet
packetList = do
  _ <- Parsec.char '['
  p <- Parsec.optionMaybe packet
  rest <- Parsec.optionMaybe $ Parsec.many (Parsec.char ',' >> Parsec.spaces >> packet)
  _ <- Parsec.char ']'
  return $ case p of
    Nothing -> PacketList []
    Just p' -> case rest of
      Nothing -> PacketList [p']
      Just rest' -> PacketList (p' : rest')

packetNumber :: Parsec.Parsec T.Text () Packet
packetNumber = do
  r <- read <$> Parsec.many1 Parsec.digit
  return (PacketNumber r)

packetListEmpty :: Parsec.Parsec T.Text () Packet
packetListEmpty = do
  _ <- Parsec.char '[' >> Parsec.char ']'
  return (PacketList [])

packet :: Parsec.Parsec T.Text () Packet
packet = do
  packetNumber <|> packetList

parse :: T.Text -> Packet
parse s = case Parsec.parse packet "source" s of
  Right m -> m
  Left e -> error $ "parse error :-( " ++ show e

-- | Given examples
-- >>> compare (parse "[1,1,3,1,1]") (parse "[1,1,5,1,1]")
-- LT
-- >>> compare (parse "[[1],[2,3,4]]") (parse "[[1],4]")
-- LT
-- >>> compare (parse "[9]") (parse "[[8,7,6]]")
-- GT
-- >>> compare (parse "[[4,4],4,4]") (parse "[[4,4],4,4,4]")
-- LT
-- >>> compare (parse "[7,7,7,7]") (parse "[7,7,7]")
-- GT
-- >>> compare (parse "[]") (parse "[3]")
-- LT
-- >>> compare (parse "[[[]]]") (parse "[[]]")
-- GT
-- >>> compare (parse "[1,[2,[3,[4,[5,6,7]]]],8,9]") (parse "[1,[2,[3,[4,[5,6,0]]]],8,9]")
-- GT

-- | Examples
-- >>> s = "[1,1,3,1,1]\n[1,1,5,1,1]\n[[1],[2,3,4]]\n[[1],4]\n[9]\n[[8,7,6]]\n[[4,4],4,4]\n[[4,4],4,4,4]\n[7,7,7,7]\n[7,7,7]\n[]\n[3]\n[[[]]]\n[[]]\n[1,[2,[3,[4,[5,6,7]]]],8,9]\n[1,[2,[3,[4,[5,6,0]]]],8,9]"
-- >>> part1 s
-- 13
-- >>> part2 s
-- 140
part1 :: T.Text -> Int
part1 =
  sum
    . map fst
    . filter
      ( \case
          (_, [one, two]) -> (parse one < parse two)
          _ -> error "invalid"
      )
    . zip [1 :: Int, 2 ..]
    . chunksOf 2
    . filter
      (/= T.empty)
    . T.splitOn "\n"

part2 :: T.Text -> Int
part2 =
  product
    . map fst
    . filter (\(_, p) -> p == parse "[[2]]" || p == parse "[[6]]")
    . zip [1, 2 ..]
    . sort
    . (++) [divider1, divider2]
    . map parse
    . filter (/= T.empty)
    . T.splitOn "\n"
  where
    divider1 = parse "[[2]]"
    divider2 = parse "[[6]]"
