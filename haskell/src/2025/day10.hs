module Day10 where

import Data.Bits (Bits (shiftL, xor, (.|.)))
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Word (Word32)
import Search (dijkstraSearch)
import Util (unsnoc)

data Machine = Machine Word32 [[Int]] [Int] deriving (Show)

-- | parseLights
-- >>> parseLights "[.##.]"
-- 6
-- >>> parseLights "[...##.]"
-- 24
parseLights :: T.Text -> Word32
parseLights t =
  snd $
    T.foldl'
      ( \(n, mask) c ->
          case c of
            '.' -> (n + 1, mask)
            '#' -> (n + 1, (1 `shiftL` n) .|. mask)
            _ -> error "invalid"
      )
      (0, 0)
      (T.dropEnd 1 $ T.drop 1 t)

-- | pushButtons
-- >>> pushButtons [3] (parseLights "[.##.]")
-- 14
-- >>> (pushButtons [0, 3, 4] (parseLights "[#.....]"), parseLights "[...##.]")
-- (24,24)
pushButtons :: [Int] -> Word32 -> Word32
pushButtons buttons word =
  foldr (\n w -> (1 `shiftL` n) `xor` w) word buttons

-- | parseButtons
-- >>> parseButtons (T.splitOn " " "(3) (1,3) (2) (2,3) (0,2) (0,1)")
-- [[3],[1,3],[2],[2,3],[0,2],[0,1]]
parseButtons :: [T.Text] -> [[Int]]
parseButtons =
  map (\t -> read . T.unpack <$> T.splitOn "," (T.dropEnd 1 $ T.drop 1 t))

-- | parseJoltage
-- >>> parseJoltage "{3,5,4,7}"
-- [3,5,4,7]
parseJoltage :: T.Text -> [Int]
parseJoltage = map (read . T.unpack) . T.splitOn "," . T.dropEnd 1 . T.drop 1

-- | parseMachine
-- >>> parseMachine "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
-- Machine 6 [[3],[1,3],[2],[2,3],[0,2],[0,1]] [3,5,4,7]
parseMachine :: T.Text -> Machine
parseMachine t =
  Machine
    (parseLights (head h))
    (parseButtons (tail h))
    (parseJoltage joltage)
  where
    (h, joltage) = fromJust $ unsnoc $ T.splitOn " " t

-- I think this is just search
minPresses :: Machine -> Int
minPresses (Machine goal buttonList _) =
  snd $
    fromJust $
      snd $
        dijkstraSearch
          0
          ( \w ->
              map (`pushButtons` w) buttonList
          )
          (== goal)

-- | part1
-- >>> s = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
-- >>> part1 s
-- 7
part1 :: T.Text -> Int
part1 t = sum $ minPresses . parseMachine <$> T.splitOn "\n" t

part2 :: T.Text -> Int
part2 _ = 1