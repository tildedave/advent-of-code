{-# LANGUAGE MultiWayIf #-}

module Day14 where

import Data.Bifunctor (bimap)
import Data.Foldable qualified as S
import Data.List (unfoldr)
import Data.Set qualified as S
import Data.Text qualified as T
import Debug.Trace (traceShow)
import Util (Coord2d, add2, compareInt)

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
    (dx, dy) = bimap (compareInt (fst c2)) (compareInt (snd c2)) c1

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

-- | Examples
-- >>> s = "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9"
-- >>> part1 s
-- 24
-- >>> part2 s
-- 93
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

-- to speed this up, return the previous sand location too.
-- not working, OK.  plane is landing.
sandFallPart2 :: Int -> Coord2d -> Coord2d -> S.Set Coord2d -> (Coord2d, Coord2d)
sandFallPart2 cutoff (x, y) prev blocked =
  if y == cutoff
    then
      ((x, y), prev)
    else
      let next = filter (\c -> not $ c `S.elem` blocked) [down, downLeft, downRight]
       in (if null next then ((x, y), prev) else sandFallPart2 cutoff (head next) (x, y) blocked)
  where
    down = (x, y + 1)
    downLeft = (x - 1, y + 1)
    downRight = (x + 1, y + 1)

part2 :: T.Text -> Int
part2 t =
  (+ 1) $
    length $
      unfoldr
        ( \(walls', prev) -> case sandFallPart2 cutoff prev prev walls' of
            ((500, 0), _) -> Nothing
            (next, prev') -> Just (next, (S.insert next walls', prev'))
        )
        (walls, (500, 0))
  where
    walls = foldr (S.union . parseLine) S.empty (T.splitOn "\n" t)
    cutoff = abyssY walls + 1