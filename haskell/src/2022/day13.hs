module Day13 where

import Data.Text qualified as T
import Text.Parsec ((<|>))
import Text.Parsec qualified as Parsec

data Packet = PacketList [Packet] | PacketNumber Int deriving (Show)

packetListSingleton :: Parsec.Parsec T.Text () Packet
packetListSingleton = do
  _ <- Parsec.char '['
  p <- packet
  _ <- Parsec.char ']'
  return (PacketList [p])

packetListMany :: Parsec.Parsec T.Text () Packet
packetListMany = do
  _ <- Parsec.char '['
  p <- packet
  rest <- Parsec.many (Parsec.char ',' >> Parsec.spaces >> packet)
  _ <- Parsec.char ']'
  return (PacketList (p : rest))

packetNumber :: Parsec.Parsec T.Text () Packet
packetNumber = do
  r <- read <$> Parsec.many1 Parsec.digit
  return (PacketNumber r)

packet :: Parsec.Parsec T.Text () Packet
packet = do
  packetNumber <|> packetListMany <|> packetListSingleton

parse :: T.Text -> Packet
parse s = case Parsec.parse packet "source" s of
  Right m -> m
  Left e -> error $ "parse error :-( " ++ show e

-- packetList = do
--   _ <- Parsec.char '['

--   _ <- Parsec.char ']'
--   return PacketList []

part1 :: T.Text -> Int
part1 _ = 1

part2 :: T.Text -> Int
part2 _ = 1
