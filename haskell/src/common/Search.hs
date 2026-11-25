{-# LANGUAGE ScopedTypeVariables #-}

module Search where

import Control.Monad.Identity (Identity (runIdentity))
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

dijkstraSearchM :: forall a m. (Ord a, Monad m) => a -> (a -> m [a]) -> (a -> m Bool) -> (a -> m Bool) -> m (M.Map a Int, Maybe (a, Int))
dijkstraSearchM start getNeighbors (isGoal :: a -> m Bool) (shouldPrune :: a -> m Bool) =
  loop
    ( M.singleton start (0 :: Int),
      S.empty,
      H.fromList [(0 :: Int, start)]
    )
  where
    loop :: DijkstraState a -> m (M.Map a Int, Maybe (a, Int))
    loop (distances, visited, queue) =
      case H.viewHead queue of
        -- apparently pure is the cool new way to write return
        Nothing -> pure (distances, Nothing)
        Just (d, next) ->
          if
            | next `S.member` visited -> loop (distances, visited, H.drop 1 queue)
            | otherwise -> do
                prune <- shouldPrune next
                if prune
                  then
                    loop (distances, visited, H.drop 1 queue)
                  else do
                    goal <- isGoal next
                    if goal
                      then
                        pure (distances, Just (next, fromJust $ M.lookup next distances))
                      else do
                        neighbors <- getNeighbors next
                        loop $
                          foldr
                            ( \neighbor state ->
                                if
                                  | isVisitedState neighbor state -> state
                                  | isNodeCloser neighbor d state -> addNextState neighbor (d + 1) state
                                  | otherwise -> state
                            )
                            (distances, S.insert next visited, H.drop 1 queue)
                            neighbors

dijkstraSearch :: (Ord a) => a -> (a -> [a]) -> (a -> Bool) -> (a -> Bool) -> (M.Map a Int, Maybe (a, Int))
dijkstraSearch start getNeighbors isGoal shouldCutoff =
  runIdentity $ dijkstraSearchM start (pure . getNeighbors) (pure . isGoal) (pure . shouldCutoff)
