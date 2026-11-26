module Day16 where

import Control.Monad.Identity (Identity)
import Control.Monad.State (StateT, gets, modify, runState)
import Data.Foldable (maximumBy)
import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust, mapMaybe)
import Data.Ord (comparing)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Void
import Debug.Trace (trace)
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
  { position :: (T.Text, Int),
    elephantPosition :: (T.Text, Int),
    time :: Int,
    openValves :: S.Set T.Text,
    totalPressureReleased :: Int,
    isPart2 :: Bool
  }
  deriving (Show, Eq, Ord)

startState :: Bool -> SearchState
startState isPart2 =
  SearchState
    { position = ("AA", 0),
      elephantPosition = ("AA", 0),
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
   in ss
        { time = endTime,
          totalPressureReleased = (pressureReleased * remainingTime) + (totalPressureReleased ss)
        }
  where
    pressureReleased = totalFlow config ss

-- | standPat
-- >>> config = ProblemConfig { allValves = S.fromList ["AA"], flowMap = M.fromList ([("AA", 5)]), endTime = 30, distanceMatrix = M.empty }
-- >>> standPat config (SearchState { position = ("AA", 0), elephantPosition = ("AA", 0), time = 29, openValves = S.fromList ["AA"], totalPressureReleased = 0, isPart2 = False})
-- SearchState {position = ("AA",0), elephantPosition = ("AA",0), time = 30, openValves = fromList ["AA"], totalPressureReleased = 5, isPart2 = False}
advance :: ProblemConfig -> SearchState -> SearchState
advance config ss@(SearchState (dest, t') (eleDest, et') time openValves totalPressureReleased isPart2) =
  if isPart2
    then
      -- OK so advancing a state needs to move whoever is currently moving forward.
      -- if someone is not moving, then they can stay where they are, that is fine.
      -- I can do this with a conditional I guess.
      if
        | t' == 0 ->
            ss
              { position = (dest, t'),
                elephantPosition = (eleDest, 0),
                openValves = S.insert eleDest openValves,
                time = time + et',
                totalPressureReleased = totalPressureReleased + pressureReleased * et'
              }
        | et' == 0 ->
            ss
              { position = (dest, 0),
                elephantPosition = (eleDest, 0),
                openValves = S.insert dest openValves,
                time = time + t',
                totalPressureReleased = totalPressureReleased + pressureReleased * t'
              }
        | otherwise ->
            let dt = min t' et'
             in ss
                  { position = (dest, t' - dt),
                    elephantPosition = (eleDest, et' - dt),
                    time = time + dt,
                    openValves = openValves & (if t' == dt then S.insert dest else id) & (if et' == dt then S.insert eleDest else id),
                    totalPressureReleased = totalPressureReleased + pressureReleased * dt
                  }
    else
      ss
        { position = (dest, 0),
          time = time + t',
          openValves = if currentFlow > 0 then (S.insert dest openValves) else openValves,
          totalPressureReleased = totalPressureReleased + pressureReleased * t'
        }
  where
    pressureReleased = totalFlow config ss
    currentFlow = fromJust $ M.lookup dest (flowMap config)

shouldVisit :: ProblemConfig -> SearchState -> T.Text -> Int -> Bool
shouldVisit config ss other dt =
  if
    -- don't go somewhere that will run out of time
    | (time ss) + dt > (endTime config) -> False
    -- don't go somewhere that is already open
    | S.member other (openValves ss) -> False
    -- don't go to valves that have no flow rate
    | fromJust (M.lookup other (flowMap config)) == 0 -> False
    -- no reason to travel to same place as partner
    | (other == (fst (position ss))) || (other == (fst (elephantPosition ss))) -> False
    | otherwise -> True

neighbors :: ProblemConfig -> SearchState -> [SearchState]
neighbors config@(ProblemConfig allValves _ endTime distanceMatrix) ss =
  if
    | (time ss) == endTime -> [] -- we'll need to add one more round of flow release to figure out the number to input
    | S.isSubsetOf allValves (openValves ss) -> [standPat config ss]
    | otherwise ->
        standPat config ss : finalMoves
  where
    (p, pt) = (position ss)
    -- (p', pt') = (elephantPosition ss)
    -- OK we need to convert [ss] --> [next ss (my positions)] --> [next ss (my + elephant positions)]
    -- if we CAN move, we always want to move, right?  no, I don't think so.
    -- if we're not careful it's possible we'll filter out any move
    myMoves =
      ss -- represent us not moving here (elephant may move)
        : if not (pt == 0)
          then []
          else
            ( mapMaybe
                ( \other ->
                    ( let dist = fromJust $ M.lookup (p, other) distanceMatrix
                          dt = dist + 1
                       in if shouldVisit config ss other dt
                            then
                              (Just $ (ss {position = (other, dt)}))
                            else Nothing
                    )
                )
                (S.toList $ (S.difference allValves (openValves ss)))
            )
    partnerMoves =
      if not (isPart2 ss)
        then
          myMoves
        else
          concatMap
            ( \ss' ->
                ss' -- represent elephant not moving
                  : if not (snd (elephantPosition ss') == 0)
                    then []
                    else
                      ( mapMaybe
                          ( \other ->
                              ( let (p', _) = (elephantPosition ss')
                                    dist = fromJust $ M.lookup (p', other) distanceMatrix
                                    dt = dist + 1
                                 in if shouldVisit config ss' other dt
                                      then
                                        Just $ (ss' {elephantPosition = (other, dt)})
                                      else Nothing
                              )
                          )
                          (S.toList $ (S.difference allValves (openValves ss')))
                      )
            )
            myMoves
    finalMoves =
      mapMaybe
        ( \ss' ->
            if
              | fst (position ss') == fst (elephantPosition ss') -> Nothing
              | (time ss') == (time ss) -> Nothing
              | otherwise -> Just ss'
        )
        (map (advance config) partnerMoves)

-- (trace ("partner moves " ++ (show partnerMoves)) (map (advance config) partnerMoves))

-- for my Clojure solution I did an all pairs shortest paths on the tunnels to
-- eliminate intermediate search states, that was pretty smart.  it feels
-- important to do here too.

-- | parseLine
-- >>> parseTest parseLine "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
-- ("AA",0,["DD","II","BB"])
-- >>> parseTest parseLine "Valve HH has flow rate=22; tunnel leads to valve GG"
-- ("HH",22,["GG"])
shouldCutoff :: SearchState -> StateT (Map (S.Set T.Text, (T.Text, Int), (T.Text, Int), Int) (Map Int Int)) Identity Bool
shouldCutoff ss = do
  let hash = ((openValves ss), (position ss), (elephantPosition ss), (time ss))
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

part1 :: T.Text -> Int
part1 t =
  let config = parseProblemConfig 30 (T.splitOn "\n" t)
      ((final, _), _) =
        runState
          ( dijkstraSearchM
              (startState False)
              (pure . (neighbors config))
              (pure . const False)
              shouldCutoff
          )
          (M.empty)
      maxPressureState = maximumBy (comparing totalPressureReleased) (M.keys final)
   in ((totalPressureReleased maxPressureState) + (totalFlow config maxPressureState))

-- | part1
-- >>> part1 "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\nValve BB has flow rate=13; tunnels lead to valves CC, AA\nValve CC has flow rate=2; tunnels lead to valves DD, BB\nValve DD has flow rate=20; tunnels lead to valves CC, AA, EE\nValve EE has flow rate=3; tunnels lead to valves FF, DD\nValve FF has flow rate=0; tunnels lead to valves EE, GG\nValve GG has flow rate=0; tunnels lead to valves FF, HH\nValve HH has flow rate=22; tunnel leads to valve GG\nValve II has flow rate=0; tunnels lead to valves AA, JJ\nValve JJ has flow rate=21; tunnel leads to valve II"
-- 1651

-- OK, I how I did part2 is by converting traveling and opening a flow into a
-- path.  I can do something like that again, sure.

part2 :: T.Text -> Int
part2 t =
  let config = parseProblemConfig 26 (T.splitOn "\n" t)
      ((final, _), _) =
        runState
          ( dijkstraSearchM
              (startState True)
              (pure . (neighbors config))
              (pure . const False)
              shouldCutoff
          )
          (M.empty)
      maxPressureState = maximumBy (comparing totalPressureReleased) (M.keys final)
   in ((totalPressureReleased maxPressureState) + (totalFlow config maxPressureState))

-- | part2
-- >>> part2 "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\nValve BB has flow rate=13; tunnels lead to valves CC, AA\nValve CC has flow rate=2; tunnels lead to valves DD, BB\nValve DD has flow rate=20; tunnels lead to valves CC, AA, EE\nValve EE has flow rate=3; tunnels lead to valves FF, DD\nValve FF has flow rate=0; tunnels lead to valves EE, GG\nValve GG has flow rate=0; tunnels lead to valves FF, HH\nValve HH has flow rate=22; tunnel leads to valve GG\nValve II has flow rate=0; tunnels lead to valves AA, JJ\nValve JJ has flow rate=21; tunnel leads to valve II"
-- 1707
