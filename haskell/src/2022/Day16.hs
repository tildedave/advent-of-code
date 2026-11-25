module Day16 where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust, mapMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Void
import Graph (AdjacencyMatrix, DistanceMatrix)
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

parseAdjacencyMatrix :: [T.Text] -> (FlowMapping, AdjacencyMatrix T.Text)
parseAdjacencyMatrix l =
  foldr
    ( \(valve, flowRate, connections) (flowMap, adjacencyMap) ->
        ( M.insert valve flowRate flowMap,
          M.insert valve connections adjacencyMap
        )
    )
    (M.empty, M.empty)
    $ map lineToAdjacency l

data SearchState = SearchState
  { position :: T.Text,
    time :: Int,
    openValves :: S.Set T.Text,
    totalPressureReleased :: Int
  }
  deriving (Show)

startState :: SearchState
startState = SearchState {position = "AA", time = 1, openValves = S.empty, totalPressureReleased = 0}

totalFlow :: FlowMapping -> S.Set T.Text -> Int
totalFlow adjancencyMatrix openValves =
  S.fold
    (\valve acc -> acc + (fromJust $ M.lookup valve adjancencyMatrix))
    0
    openValves

data ProblemConfig = ProblemConfig
  { allValves :: [T.Text],
    flowMap :: FlowMapping,
    endTime :: Int,
    distanceMatrix :: DistanceMatrix T.Text
  }

standPat :: ProblemConfig -> SearchState -> SearchState
standPat (ProblemConfig _ flowMap endTime _) ss =
  let remainingTime = endTime - (time ss)
   in ss
        { time = endTime,
          totalPressureReleased = (pressureReleased * (remainingTime + 1)) + (totalPressureReleased ss)
        }
  where
    pressureReleased = totalFlow flowMap (openValves ss)

-- | standPat
-- >>> config = ProblemConfig { allValves = ["AA"], flowMap = M.fromList ([("AA", 5)]), endTime = 30, distanceMatrix = M.empty }
-- >>> standPat config  (SearchState { position = "AA", time = 29, openValves = S.fromList ["AA"], totalPressureReleased = 0})
-- SearchState {position = "AA", time = 30, openValves = fromList ["AA"], totalPressureReleased = 10}
openValve :: SearchState -> SearchState
openValve ss = ss {openValves = (S.insert (position ss) (openValves ss)), time = (time ss) + 1}

neighbors :: ProblemConfig -> SearchState -> [SearchState]
neighbors config@(ProblemConfig allValves flowMap endTime distanceMatrix) ss =
  if (time ss) == endTime
    then
      [ss {time = endTime + 1}] -- we'll use > endTime as our goal checker
    else
      if (S.member (position ss) (openValves ss))
        then
          standPat config ss : travelMoves
        else
          standPat config ss : (openValve ss) : travelMoves
  where
    pressureReleased = totalFlow flowMap (openValves ss)
    travelMoves =
      mapMaybe
        ( \other ->
            let dist = fromJust $ M.lookup ((position ss), other) distanceMatrix
                nextTime = (time ss) + dist
             in if nextTime > endTime
                  then
                    -- moving to somewhere and doing nothing is the same as "standing pat"
                    Nothing
                  else
                    Just
                      SearchState
                        { position = other,
                          time = nextTime,
                          openValves = (openValves ss),
                          totalPressureReleased = (pressureReleased + (totalPressureReleased ss)) * dist
                        }
        )
        allValves

-- for my Clojure solution I did an all pairs shortest paths on the tunnels to
-- eliminate intermediate search states, that was pretty smart.  it feels
-- important to do here too.

-- | parseLine
-- >>> parseTest parseLine "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
-- ("AA",0,["DD","II","BB"])
-- >>> parseTest parseLine "Valve HH has flow rate=22; tunnel leads to valve GG"
-- ("HH",22,["GG"])
part1 :: T.Text -> Int
part1 _ = 1

part2 :: T.Text -> Int
part2 _ = 1
