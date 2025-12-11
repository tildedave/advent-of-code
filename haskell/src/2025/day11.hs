{-# LANGUAGE LambdaCase #-}

module Day11 where

import Control.Monad (foldM)
import Control.Monad.State (State, evalState, gets, modify)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Debug.Trace (traceShow)

-- all paths out is a kind of memoization exercise.  dynamic programming from the out connection.

type IncomingConnections = Map T.Text [T.Text]

-- | parseLine
-- >>> s = "aaa: you hhh\nyou: bbb ccc\nbbb: ddd eee\nccc: ddd eee fff\nddd: ggg\neee: out\nfff: out\nggg: out\nhhh: ccc fff iii\niii: out"
-- >>> foldr parseLine (Map.empty) (T.splitOn "\n" s)
-- fromList [("bbb",["you"]),("ccc",["you","hhh"]),("ddd",["bbb","ccc"]),("eee",["bbb","ccc"]),("fff",["ccc","hhh"]),("ggg",["ddd"]),("hhh",["aaa"]),("iii",["hhh"]),("out",["eee","fff","ggg","iii"]),("you",["aaa"])]
parseLine :: T.Text -> IncomingConnections -> IncomingConnections
parseLine t m =
  foldr
    ( Map.alter
        ( \case
            Nothing -> Just [out]
            Just xs -> Just $ out : xs
        )
    )
    m
    (T.splitOn " " rhs)
  where
    (out, rhs) = case T.splitOn ": " t of
      [l, r] -> (l, r)
      _ -> error "invalid parse"

type IncomingConnectionsCount = Map T.Text Int

pathsBetween :: IncomingConnections -> T.Text -> T.Text -> Int
pathsBetween conns source dest = evalState (numPathsOut conns dest) (Map.fromList [(source, 1)])

numPathsOut :: IncomingConnections -> T.Text -> State IncomingConnectionsCount Int
numPathsOut conns label = do
  gets (Map.lookup label) >>= \case
    Just n -> return n
    Nothing -> do
      result <-
        foldM
          (\n conn -> fmap (+ n) (numPathsOut conns conn))
          0
          (Map.findWithDefault [] label conns)

      modify (Map.insert label result)
      return result

-- | Examples
-- >>> s = "aaa: you hhh\nyou: bbb ccc\nbbb: ddd eee\nccc: ddd eee fff\nddd: ggg\neee: out\nfff: out\nggg: out\nhhh: ccc fff iii\niii: out"
-- >>> part1 s
-- 5
part1 :: T.Text -> Int
part1 t =
  pathsBetween conns "you" "out"
  where
    conns = foldr parseLine Map.empty (T.splitOn "\n" t)

-- my part1 answer is 534.  could have enumerated them all.  but, for my input, there are ~74669640838620730 paths from svr to out. so no enumeration :-)
-- paths out look like:
-- svr -> dac -> fft -> out
-- svr -> fft -> dac -> out
-- the idea that we could visit in either order indicates that we're going to get into double-counting issues.
-- well, let's just start coding and see if that helps us understand this better.
-- graphviz might help
-- number of paths between dac and svr *avoiding* fft

connectionsToDot :: IncomingConnections -> T.Text
connectionsToDot conn =
  T.pack $
    "digraph G {\n"
      ++ intercalate
        "\n"
        ( concatMap
            ( \(label, incoming) ->
                map
                  ( \inc ->
                      "\t" ++ show label ++ " -> " ++ show inc ++ ";"
                  )
                  incoming
            )
            (Map.toList conn)
        )
      ++ "}"

-- looking at the graph there are several "networks", dac and fft are just random nodes in them.  there are no loops.
-- in my input it is svr -> fft -> dac -> out.
-- so the result is paths between svr -> fft, paths between fft -> dac, paths between dac -> out.
-- (my code is opposite so whatever)
-- it feels like my current code handles this already?
-- answer based on this is is too low

part2 :: T.Text -> Int
part2 t =
  paths "svr" "fft" * paths "fft" "dac" * paths "dac" "out"
  where
    paths = pathsBetween (foldr parseLine Map.empty (T.splitOn "\n" t))
