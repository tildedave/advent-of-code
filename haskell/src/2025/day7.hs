{-# LANGUAGE OverloadedStrings #-}

module Day7 where

-- import Data.Sequence (Seq (..), viewl)

import Data.List (unfoldr)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text qualified as T
import Debug.Trace (traceShow)
import Util (Coord2d, Grid, add2, bounds, gridAt, gridAt_, gridCoords, gridFind, parseGrid)

beamStep :: Grid Coord2d Char -> (S.Set Coord2d, Seq.Seq Coord2d) -> Maybe (Maybe Coord2d, (S.Set Coord2d, Seq.Seq Coord2d))
beamStep _ (_, Seq.Empty) = Nothing
beamStep grid (visited, x :<| s') = case gridAt x grid of
  Nothing -> Just (Nothing, (visited, s'))
  Just '.' -> Just (Nothing, (visited, s' :|> add2 x (0, 1)))
  Just 'S' -> Just (Nothing, (visited, s' :|> add2 x (0, 1))) -- boo Haskell not doing multi-matches
  Just '^' ->
    Just
      ( Just x,
        foldr
          ( \dx (visited', s_) ->
              let next = add2 x dx
               in if S.member next visited'
                    then
                      (visited', s_)
                    else
                      (S.insert next visited', s_ :|> next)
          )
          (visited, s')
          [(1, 0), (-1, 0)]
      )
  _ -> error "impossible"

-- | Examples
-- >>> s = ".......S.......\n...............\n.......^.......\n...............\n......^.^......\n...............\n.....^.^.^.....\n...............\n....^.^...^....\n...............\n...^.^...^.^...\n...............\n..^...^.....^..\n...............\n.^.^.^.^.^...^.\n...............\n"
-- >>> part1 s
-- 21
part1 :: T.Text -> Int
part1 t =
  length $ foldr S.insert S.empty $ catMaybes $ unfoldr (beamStep grid) (S.empty, Seq.singleton (gridFind 'S' grid))
  where
    grid = parseGrid id t

-- part 2 is just dynamic programming

-- | numTimelines
-- >>> s = ".......S.......\n...............\n.......^.......\n...............\n......^.^......\n...............\n.....^.^.^.....\n...............\n....^.^...^....\n...............\n...^.^...^.^...\n...............\n..^...^.....^..\n...............\n.^.^.^.^.^...^.\n...............\n"
-- >>> g = parseGrid id s
-- >>> numTimelines g (M.empty) (7, 0)
-- (1,fromList [((7,0),1)])
-- >>> numTimelines g (M.empty) (7, 1)
-- (1,fromList [((7,1),1)])
numTimelines :: Grid Coord2d Char -> M.Map Coord2d Int -> Coord2d -> (Int, M.Map Coord2d Int)
numTimelines grid memory coord =
  case M.lookup coord memory of
    Just n -> (n, memory)
    Nothing ->
      case gridAt coord grid of
        Nothing -> (0, memory)
        Just 'S' -> (1, M.insert coord 1 memory)
        Just '^' -> (0, M.insert coord 0 memory) -- impossible
        _ ->
          let up = add2 (0, -1) coord
              (nu, memory1) = numTimelines grid memory up
              left = add2 (-1, 0) coord
              (nl, memory2) = case gridAt left grid of
                Just '^' -> numTimelines grid memory1 (add2 (0, -1) left)
                _ -> (0, memory1)
              right = add2 (1, 0) coord
              (nr, memory3) = case gridAt right grid of
                Just '^' -> numTimelines grid memory2 (add2 (0, -1) right)
                _ -> (0, memory2)
           in (nu + nl + nr, M.insert coord (nu + nl + nr) memory3)

bottomCoords :: Grid Coord2d Char -> [Coord2d]
bottomCoords grid =
  filter (\c -> snd c == ymax) (gridCoords grid)
  where
    (_, ymax) = bounds grid

part2 :: T.Text -> Int
part2 t =
  sum $ fst . numTimelines grid M.empty <$> bottomCoords grid
  where
    grid = parseGrid id t
