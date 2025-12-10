{-# LANGUAGE MultiWayIf #-}

module Day14 where

import Data.Foldable qualified as S
import Data.List (unfoldr)
import Data.Set qualified as S
import Data.Text qualified as T
import Util (Coord2d, coordRange, parseCoord)

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
sandFallPart2 :: Int -> [Coord2d] -> S.Set Coord2d -> (Coord2d, [Coord2d])
sandFallPart2 cutoff ((x, y) : path) blocked =
  if y == cutoff
    then
      ((x, y), path)
    else
      let next = filter (\c -> not $ c `S.elem` blocked) [down, downLeft, downRight]
       in (if null next then ((x, y), path) else sandFallPart2 cutoff (head next : ((x, y) : path)) blocked)
  where
    down = (x, y + 1)
    downLeft = (x - 1, y + 1)
    downRight = (x + 1, y + 1)
sandFallPart2 _ [] _ = error "empty path"

part2 :: T.Text -> Int
part2 t =
  (+ 1) $
    length $
      unfoldr
        ( \(walls', path) -> case sandFallPart2 cutoff path walls' of
            ((500, 0), _) -> Nothing
            (next, path') -> Just (next, (S.insert next walls', path'))
        )
        (walls, [(500, 0)])
  where
    walls = foldr (S.union . parseLine) S.empty (T.splitOn "\n" t)
    cutoff = abyssY walls + 1