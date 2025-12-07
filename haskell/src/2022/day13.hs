module Day13 where

import qualified Data.Text as T
import qualified Text.Parsec as Parsec

data Packet = PacketList [Packet] | PacketNumber Int

number :: Parsec.Parsec T.Text () Int
number = read <$> Parsec.many1 Parsec.digit

-- seems like we need to define these recursively.  I suppose a forward decl
-- is all we have to do.
-- packetList :: Parsec.Parsec T.Text () Packet
-- packet :: Parsec.Parsec T.Text () Packet
-- packetList = do
--   _ <- Parsec.char '['

--   _ <- Parsec.char ']'
--   return PacketList []

part1 :: T.Text -> Int
part1 _ = 1

part2 :: T.Text -> Int
part2 _ = 1
