{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day12 where

import Data.Heap (MinPrioHeap)
import Data.Heap qualified as H
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Set qualified as S

type DijkstraState a = (Ord a) => (M.Map a Int, S.Set a, MinPrioHeap Int a)

addNextState :: (Ord a) => a -> Int -> DijkstraState a -> DijkstraState a
addNextState node d (distances, visited, queue) =
  ( M.insert node d distances,
    visited,
    H.insert (d, node) queue
  )

distanceState :: (Ord a) => a -> DijkstraState a -> Maybe Int
distanceState node (distances, _, _) =
  M.lookup node distances

isVisitedState :: (Ord a) => a -> DijkstraState a -> Bool
isVisitedState node (_, visited, _) = node `S.member` visited

isNodeCloser :: (Ord a) => a -> Int -> DijkstraState a -> Bool
isNodeCloser node d state =
  case distanceState node state of
    Nothing -> True
    Just d' -> d + 1 < d'

dijkstraSearch :: (Ord a) => a -> (a -> [a]) -> (a -> Bool) -> (M.Map a Int, Maybe (a, Int))
dijkstraSearch start neighbors (isGoal :: a -> Bool) =
  -- Can I monad this :-)
  loop
    ( M.singleton start (0 :: Int),
      S.empty,
      H.fromList [(0 :: Int, start)]
    )
  where
    loop :: DijkstraState a -> (M.Map a Int, Maybe (a, Int))
    loop (distances, visited, queue) =
      case H.viewHead queue of
        Nothing -> (distances, Nothing)
        Just (d, next) ->
          if next `S.member` visited
            then
              loop (distances, visited, H.drop 1 queue)
            else
              if isGoal next
                then
                  (distances, Just (next, fromJust $ M.lookup next distances))
                else
                  loop $
                    foldr
                      ( \neighbor state ->
                          if
                            | isVisitedState neighbor state -> state
                            | isNodeCloser neighbor (d + 1) state -> addNextState neighbor (d + 1) state
                            | otherwise -> state
                      )
                      (distances, S.insert next visited, H.drop 1 queue)
                      (neighbors next)
