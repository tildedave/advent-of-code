{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day7 where

-- import Data.Sequence (Seq (..), viewl)

import Control.Monad (foldM)
import Control.Monad.State.Lazy (State, evalState, gets, modify)
import Data.List (unfoldr)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text qualified as T
import Util (Coord2d, Grid, add2, bounds, gridAt, gridCoords, gridFind, parseGrid)

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
-- >>> part2 s
-- 40
part1 :: T.Text -> Int
part1 t =
  length $ foldr S.insert S.empty $ catMaybes $ unfoldr (beamStep grid) (S.empty, Seq.singleton (gridFind 'S' grid))
  where
    grid = parseGrid id t

-- part 2 is just dynamic programming

numTimelines :: Grid Coord2d Char -> Coord2d -> State (M.Map Coord2d Int) Int
numTimelines grid coord = do
  gets (M.lookup coord) >>= \case
    Just n -> return n
    Nothing -> do
      l <- case gridAt coord grid of
        Nothing -> return 0
        Just 'S' -> return 1
        Just '^' -> return 0
        _ -> do
          let up = add2 (0, -1) coord
              right = add2 (1, 0) coord
              left = add2 (-1, 0) coord

          nu <- numTimelines grid up
          nl <- case gridAt left grid of
            Just '^' -> numTimelines grid (add2 (0, -1) left)
            _ -> return 0
          nr <- case gridAt right grid of
            Just '^' -> numTimelines grid (add2 (0, -1) right)
            _ -> return 0
          return (nu + nl + nr)

      modify (M.insert coord l)
      return l

bottomCoords :: Grid Coord2d Char -> [Coord2d]
bottomCoords grid =
  filter (\c -> snd c == ymax) (gridCoords grid)
  where
    (_, ymax) = bounds grid

part2 :: T.Text -> Int
part2 t =
  evalState
    ( foldM
        ( \a c -> do
            r <- numTimelines grid c
            return (r + a)
        )
        0
        (bottomCoords grid)
    )
    M.empty
  where
    grid = parseGrid id t
