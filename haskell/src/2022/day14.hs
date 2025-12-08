{-# LANGUAGE MultiWayIf #-}

module Day14 where

import Data.Foldable qualified as S
import Data.List (unfoldr)
import Data.Set qualified as S
import Data.Text qualified as T
import Util (Coord2d, add2, sub2)

-- | parseCoord
-- >>> parseCoord "498,4"
-- (498,4)
parseCoord :: T.Text -> Coord2d
parseCoord = (\l -> (head l, l !! 1)) . map (read . T.unpack) . T.splitOn ","

-- | coordRange
-- >>> coordRange (498,4) (498,6)
-- [(498,4),(498,5),(498,6)]
coordRange :: Coord2d -> Coord2d -> [Coord2d]
coordRange c1 c2 =
  takeWhile (/= c2) (iterate (add2 (dx, dy)) c1) ++ [c2]
  where
    (dx, dy) = case sub2 c1 c2 of
      (x, 0) -> if x > 0 then (-1, 0) else (1, 0)
      (0, y) -> if y > 0 then (0, -1) else (0, 1)
      (_, _) -> error "invalid range"

-- | parseLine
-- >>> parseLine "498,4 -> 498,6 -> 496,6"
-- fromList [(496,6),(497,6),(498,4),(498,5),(498,6)]
parseLine :: T.Text -> S.Set Coord2d
parseLine t =
  foldr
    (\p s -> foldr S.insert s (uncurry coordRange p))
    S.empty
    (zip coords (tail coords))
  where
    coords = parseCoord <$> T.splitOn " -> " t

abyssY :: S.Set Coord2d -> Int
abyssY = maximum . map snd . S.elems

sandFall :: Int -> Coord2d -> S.Set Coord2d -> Maybe Coord2d
sandFall cutoff (x, y) blocked =
  let next = filter (\c -> not $ c `S.elem` blocked) [down, downLeft, downRight]
   in if
        | null next -> Just (x, y)
        | y > cutoff -> Nothing
        | otherwise -> sandFall cutoff (head next) blocked
  where
    down = (x, y + 1)
    downLeft = (x - 1, y + 1)
    downRight = (x + 1, y + 1)

part1 :: T.Text -> Int
part1 t =
  length $
    unfoldr
      ( \walls' -> do
          next <- sandFall cutoff (500, 0) walls'
          return (next, S.insert next walls')
      )
      walls
  where
    walls = foldr (S.union . parseLine) S.empty (T.splitOn "\n" t)
    cutoff = abyssY walls

part2 :: T.Text -> Int
part2 _ = 1
