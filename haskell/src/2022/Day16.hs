module Day16 where

import Data.Foldable (maximumBy)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust, mapMaybe)
import Data.Ord (comparing)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Void
import Graph (DistanceMatrix, adjacencyToInitialWeights, allPairsShortestPaths)
import Search (dijkstraSearch)
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

parseProblemConfig :: Int -> [T.Text] -> ProblemConfig
parseProblemConfig endTime l =
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
        { allValves = S.delete "AA" $ S.fromList (M.keys adjacencyMatrix),
          flowMap,
          endTime,
          distanceMatrix = allPairsShortestPaths (adjacencyToInitialWeights adjacencyMatrix)
        }

data SearchState = SearchState
  { position :: T.Text,
    time :: Int,
    openValves :: S.Set T.Text,
    totalPressureReleased :: Int,
    isPart2 :: Bool
  }
  deriving (Show, Eq, Ord)

startState :: Bool -> SearchState
startState isPart2 =
  SearchState
    { position = "AA",
      time = 1,
      openValves = S.empty,
      totalPressureReleased = 0,
      isPart2 = isPart2
    }

totalFlow :: ProblemConfig -> SearchState -> Int
totalFlow (ProblemConfig _ flowMap _ _) ss =
  S.fold
    (\valve acc -> acc + (fromJust $ M.lookup valve flowMap))
    0
    (openValves ss)

standPat :: ProblemConfig -> SearchState -> SearchState
standPat config@(ProblemConfig _ _ endTime _) ss =
  let remainingTime = endTime - (time ss)
   in ss -- this ensures both us + partner finish moving
        { time = endTime,
          totalPressureReleased = (pressureReleased * remainingTime) + (totalPressureReleased ss)
        }
  where
    pressureReleased = totalFlow config ss

-- | standPat
-- >>> config = ProblemConfig { allValves = S.fromList ["AA"], flowMap = M.fromList ([("AA", 5)]), endTime = 30, distanceMatrix = M.empty }
-- >>> standPat config (SearchState { position = "AA", time = 29, openValves = S.fromList ["AA"], totalPressureReleased = 0, isPart2 = False})
-- SearchState {position = "AA", time = 30, openValves = fromList ["AA"], totalPressureReleased = 5, isPart2 = False}
shouldVisit :: ProblemConfig -> SearchState -> T.Text -> Int -> Bool
shouldVisit config ss other dt =
  if
    -- don't go somewhere that will run out of time
    | (time ss) + dt > (endTime config) -> False
    -- don't go somewhere that is already open
    | S.member other (openValves ss) -> False
    -- don't go to valves that have no flow rate
    | fromJust (M.lookup other (flowMap config)) == 0 -> False
    | otherwise -> True

neighbors :: ProblemConfig -> SearchState -> [SearchState]
neighbors config@(ProblemConfig allValves _ endTime distanceMatrix) ss =
  if
    | (time ss) == endTime -> [] -- we'll need to add one more round of flow release to figure out the number to input
    | S.isSubsetOf allValves (openValves ss) -> [standPat config ss]
    | otherwise ->
        standPat config ss : travelMoves
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
                  | fromJust (M.lookup other (flowMap config)) == 0 -> Nothing -- don't open valves that have no flow rate
                  | otherwise ->
                      -- move to somewhere and open it, vs just moving for no reason
                      Just
                        ss
                          { position = other,
                            time = (time ss) + dt,
                            openValves = (S.insert other (openValves ss)),
                            totalPressureReleased = (totalPressureReleased ss) + pressureReleased * (dist + 1)
                          }
        )
        (S.toList $ allValves)

-- (trace ("partner moves " ++ (show partnerMoves)) (map (advance config) partnerMoves))

-- for my Clojure solution I did an all pairs shortest paths on the tunnels to
-- eliminate intermediate search states, that was pretty smart.  it feels
-- important to do here too.

-- | parseLine
-- >>> parseTest parseLine "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
-- ("AA",0,["DD","II","BB"])
-- >>> parseTest parseLine "Valve HH has flow rate=22; tunnel leads to valve GG"
-- ("HH",22,["GG"])
part1 :: T.Text -> Int
part1 t =
  let config = parseProblemConfig 30 (T.splitOn "\n" t)
      (final, _) =
        ( dijkstraSearch
            (startState False)
            (neighbors config)
            (const False)
            (const False)
        )
      maxPressureState = maximumBy (comparing totalPressureReleased) (M.keys final)
   in ((totalPressureReleased maxPressureState) + (totalFlow config maxPressureState))

-- | part1
-- >>> part1 "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\nValve BB has flow rate=13; tunnels lead to valves CC, AA\nValve CC has flow rate=2; tunnels lead to valves DD, BB\nValve DD has flow rate=20; tunnels lead to valves CC, AA, EE\nValve EE has flow rate=3; tunnels lead to valves FF, DD\nValve FF has flow rate=0; tunnels lead to valves EE, GG\nValve GG has flow rate=0; tunnels lead to valves FF, HH\nValve HH has flow rate=22; tunnel leads to valve GG\nValve II has flow rate=0; tunnels lead to valves AA, JJ\nValve JJ has flow rate=21; tunnel leads to valve II"
-- 1651

-- we'll do part2 more intelligently with some tips from Reddit - basically just
-- keep track of the best "disjoint" search of open valves.
part2 :: T.Text -> Int
part2 t =
  let config = parseProblemConfig 26 (T.splitOn "\n" t)
      (final, _) =
        ( dijkstraSearch
            (startState True)
            ((neighbors config))
            (const False)
            (const False)
        )
      bestByOpenValves =
        foldl'
          ( \m ss ->
              let result = (totalPressureReleased ss) + (totalFlow config ss)
               in case (M.lookup (openValves ss) m) of
                    Nothing -> M.insert (openValves ss) result m
                    Just t' ->
                      if result > t'
                        then
                          M.insert (openValves ss) result m
                        else m
          )
          (M.empty)
          (M.keys final)
   in maximum
        [ (fromJust $ (M.lookup s1 bestByOpenValves)) + (fromJust $ (M.lookup s2 bestByOpenValves))
        | s1 <- M.keys bestByOpenValves,
          s2 <- M.keys bestByOpenValves,
          S.disjoint s1 s2
        ]

-- | part2
-- >>> part2 "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\nValve BB has flow rate=13; tunnels lead to valves CC, AA\nValve CC has flow rate=2; tunnels lead to valves DD, BB\nValve DD has flow rate=20; tunnels lead to valves CC, AA, EE\nValve EE has flow rate=3; tunnels lead to valves FF, DD\nValve FF has flow rate=0; tunnels lead to valves EE, GG\nValve GG has flow rate=0; tunnels lead to valves FF, HH\nValve HH has flow rate=22; tunnel leads to valve GG\nValve II has flow rate=0; tunnels lead to valves AA, JJ\nValve JJ has flow rate=21; tunnel leads to valve II"
-- 1707
