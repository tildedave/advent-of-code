module Day16 where

import Control.Monad.Identity (Identity)
import Control.Monad.State (StateT, gets, modify, runState)
import Data.Foldable (maximumBy)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust, mapMaybe)
import Data.Ord (comparing)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Void
import Graph (DistanceMatrix, adjacencyToInitialWeights, allPairsShortestPaths)
import Search (dijkstraSearchM)
import Text.Megaparsec
import Text.Megaparsec qualified as Parsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void T.Text

valveName :: Parser (T.Text)
valveName = T.pack <$> count 2 upperChar

parseLine :: Parser (T.Text, Int, [T.Text])
parseLine = do
  _ <- string "Valve "
  valve <- valveName
  _ <- string " has flow rate="
  flowRate <- L.decimal
  _ <- (string "; tunnels lead to valves " <|> string "; tunnel leads to valve ")
  connections <- valveName `sepBy1` (string ", ")
  return (valve, flowRate, connections)

lineToAdjacency :: T.Text -> (T.Text, Int, [T.Text])
lineToAdjacency t =
  case Parsec.parse parseLine "source" t of
    Right x -> x
    Left e -> error $ "parse error :-( " ++ show e

type FlowMapping = Map T.Text Int

data ProblemConfig = ProblemConfig
  { allValves :: S.Set T.Text,
    flowMap :: FlowMapping,
    endTime :: Int,
    distanceMatrix :: DistanceMatrix T.Text
  }

parseProblemConfig :: [T.Text] -> ProblemConfig
parseProblemConfig l =
  let (flowMap, adjacencyMatrix) =
        foldr
          ( \(valve, flowRate, connections) (fm, adjacencyMap) ->
              ( M.insert valve flowRate fm,
                M.insert valve connections adjacencyMap
              )
          )
          (M.empty, M.empty)
          $ map lineToAdjacency l
   in ProblemConfig
        { allValves = S.fromList (M.keys adjacencyMatrix),
          flowMap = flowMap,
          endTime = 30,
          distanceMatrix = allPairsShortestPaths (adjacencyToInitialWeights adjacencyMatrix)
        }

data SearchState = SearchState
  { position :: T.Text,
    time :: Int,
    openValves :: S.Set T.Text,
    totalPressureReleased :: Int
  }
  deriving (Show, Eq, Ord)

startState :: SearchState
startState = SearchState {position = "AA", time = 1, openValves = S.empty, totalPressureReleased = 0}

totalFlow :: ProblemConfig -> SearchState -> Int
totalFlow (ProblemConfig _ flowMap _ _) ss =
  S.fold
    (\valve acc -> acc + (fromJust $ M.lookup valve flowMap))
    0
    (openValves ss)

standPat :: ProblemConfig -> SearchState -> SearchState
standPat config@(ProblemConfig _ _ endTime _) ss =
  let remainingTime = endTime - (time ss)
   in ss
        { time = endTime,
          totalPressureReleased = (pressureReleased * remainingTime) + (totalPressureReleased ss)
        }
  where
    pressureReleased = totalFlow config ss

-- | standPat
-- >>> config = ProblemConfig { allValves = ["AA"], flowMap = M.fromList ([("AA", 5)]), endTime = 30, distanceMatrix = M.empty }
-- >>> standPat config  (SearchState { position = "AA", time = 29, openValves = S.fromList ["AA"], totalPressureReleased = 0})
-- SearchState {position = "AA", time = 30, openValves = fromList ["AA"], totalPressureReleased = 5}
openValve :: SearchState -> SearchState
openValve ss = ss {openValves = (S.insert (position ss) (openValves ss)), time = (time ss) + 1}

neighbors :: ProblemConfig -> SearchState -> [SearchState]
neighbors config@(ProblemConfig allValves flowMap endTime distanceMatrix) ss =
  if
    | S.isSubsetOf allValves (openValves ss) -> [standPat config ss]
    | (time ss) == endTime -> [] -- we'll need to add one more round of flow release to figure out the number to input
    | otherwise ->
        if (S.member (position ss) (openValves ss))
          then
            standPat config ss : travelMoves
          else
            standPat config ss : (openValve ss) : travelMoves
  where
    pressureReleased = totalFlow config ss
    travelMoves =
      mapMaybe
        ( \other ->
            let dist = fromJust $ M.lookup ((position ss), other) distanceMatrix
                dt = dist + 1
             in if
                  -- moving to somewhere and doing nothing is the same as "standing pat"
                  | (time ss) + dt > endTime -> Nothing
                  | S.member other (openValves ss) -> Nothing -- don't go somewhere that doesn't matter
                  | fromJust (M.lookup other flowMap) == 0 -> Nothing -- don't open valves that have no flow rate
                  | otherwise ->
                      -- move to somewhere and open it, vs just moving for no reason
                      Just
                        SearchState
                          { position = other,
                            time = (time ss) + dt,
                            openValves = (S.insert other (openValves ss)),
                            totalPressureReleased = (totalPressureReleased ss) + pressureReleased * (dist + 1)
                          }
        )
        (S.toList $ allValves)

-- for my Clojure solution I did an all pairs shortest paths on the tunnels to
-- eliminate intermediate search states, that was pretty smart.  it feels
-- important to do here too.

-- | parseLine
-- >>> parseTest parseLine "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
-- ("AA",0,["DD","II","BB"])
-- >>> parseTest parseLine "Valve HH has flow rate=22; tunnel leads to valve GG"
-- ("HH",22,["GG"])
shouldCutoff :: SearchState -> StateT (Map (S.Set T.Text, T.Text, Int) (Map Int Int)) Identity Bool
shouldCutoff =
  ( \ss -> do
      let hash = ((openValves ss), (position ss), (time ss))
      -- This stores a time to best pressure map.  then if you're at a position
      -- with certain open valves at time t and there's a <= t time
      -- with a better pressure, cutoff
      curr <- gets (M.lookup hash)
      case curr of
        Nothing -> do
          modify (M.insert hash (M.singleton (time ss) (totalPressureReleased ss)))
          pure False
        Just timeMap ->
          do
            let pressures = (mapMaybe ((flip M.lookup) timeMap) [1 .. (time ss)])
            if any (\v -> v > (totalPressureReleased ss)) pressures
              then
                pure True
              else do
                modify (M.adjust (M.insert (time ss) (totalPressureReleased ss)) hash)
                pure False
  )

part1 :: T.Text -> Int
part1 t =
  let config = parseProblemConfig (T.splitOn "\n" t)
      ((final, _), _) =
        runState
          ( dijkstraSearchM
              startState
              (pure . (neighbors config))
              (pure . const False)
              shouldCutoff
          )
          (M.empty)
      maxPressureState = maximumBy (comparing totalPressureReleased) (M.keys final)
   in ((totalPressureReleased maxPressureState) + (totalFlow config maxPressureState))

-- | part1
-- >>> part1 "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\nValve BB has flow rate=13; tunnels lead to valves CC, AA\nValve CC has flow rate=2; tunnels lead to valves DD, BB\nValve DD has flow rate=20; tunnels lead to valves CC, AA, EE\nValve EE has flow rate=3; tunnels lead to valves FF, DD\nValve FF has flow rate=0; tunnels lead to valves EE, GG\nValve GG has flow rate=0; tunnels lead to valves FF, HH\nValve HH has flow rate=22; tunnel leads to valve GG\nValve II has flow rate=0; tunnels lead to valves AA, JJ\nValve JJ has flow rate=21; tunnel leads to valve II"
-- 1
part2 :: T.Text -> Int
part2 _ = 1
