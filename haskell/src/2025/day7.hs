module Day7 where

-- import Data.Sequence (Seq (..), viewl)

import Data.List (unfoldr)
import Data.Maybe (catMaybes)
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text qualified as T
import Util (Coord2d, Grid, add2, gridAt, gridFind, parseGrid)

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
          ( \dx (visited', s') ->
              let next = add2 x dx
               in if S.member next visited'
                    then
                      (visited', s')
                    else
                      (S.insert next visited', s' :|> next)
          )
          (visited, s')
          [(1, 0), (-1, 0)]
      )
  _ -> error "impossible"

part1 :: T.Text -> Int
part1 t =
  length $ foldr S.insert S.empty $ catMaybes $ unfoldr (beamStep grid) (S.empty, Seq.singleton (gridFind 'S' grid))
  where
    grid = parseGrid id t

part2 :: T.Text -> Int
part2 _ = 1
