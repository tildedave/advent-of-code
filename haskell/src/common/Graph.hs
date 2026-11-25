module Graph where

import Data.Map qualified as M
import Data.Set qualified as S

--   match (Map.find m (i, k), Map.find m (k, j)) with
--   | Some d1, Some d2 ->
--       Map.update m (i, j) ~f:(function
--         | None -> d1 + d2
--         | Some d' -> if d1 + d2 < d' then d1 + d2 else d')
--   | _ -> m)

type AdjacencyMatrix a = M.Map a [a]

type DistanceMatrix a = M.Map (a, a) Int

allPairsShortestPaths :: (Ord a) => DistanceMatrix a -> DistanceMatrix a
allPairsShortestPaths m =
  foldl'
    ( \dist (k, i, j) ->
        case (M.lookup (i, k) dist, M.lookup (k, j) dist) of
          (Just d1, Just d2) ->
            M.alter
              ( \case
                  Nothing -> Just (d1 + d2)
                  Just d' | d1 + d2 < d' -> Just (d1 + d2)
                  old -> old
              )
              (i, j)
              dist
          _ -> dist
    )
    m
    indices
  where
    nodes = S.toList $ S.fromList ((map fst $ M.keys m) ++ (map snd $ M.keys m))
    indices = do
      k <- nodes
      i <- nodes
      j <- nodes
      pure (k, i, j)

adjacencyToInitialWeights :: (Ord a) => AdjacencyMatrix a -> DistanceMatrix a
adjacencyToInitialWeights m =
  M.fromList [((x, y), 1) | (x, ys) <- M.toList m, y <- ys]
