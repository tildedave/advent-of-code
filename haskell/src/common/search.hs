{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Search where

import Data.Heap (MinPrioHeap)
import Data.Map qualified as M
import Data.Heap qualified as H
import Data.Set qualified as S
import Data.Maybe (fromJust)

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

dijkstraSearch :: (Ord a) => a -> (a -> [a]) -> (a -> Bool) -> (a -> Bool) -> (M.Map a Int, Maybe (a, Int))
dijkstraSearch start getNeighbors (isGoal :: a -> Bool) (shouldPrune :: a -> Bool) =
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
          if
            | next `S.member` visited -> loop (distances, visited, H.drop 1 queue)
            | shouldPrune next -> loop (distances, visited, H.drop 1 queue)
            | isGoal next -> (distances, Just (next, fromJust $ M.lookup next distances))
            | otherwise ->
                loop $
                  foldr
                    ( \neighbor state ->
                        if
                          | isVisitedState neighbor state -> state
                          | isNodeCloser neighbor (d + 1) state -> addNextState neighbor (d + 1) state
                          | otherwise -> state
                    )
                    (distances, S.insert next visited, H.drop 1 queue)
                    (getNeighbors next)
