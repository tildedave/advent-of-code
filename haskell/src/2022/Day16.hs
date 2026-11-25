module Day16 where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Void
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

type AdjacencyMatrix = Map T.Text (Int, [T.Text])

parseAdjacencyMatrix :: [T.Text] -> AdjacencyMatrix
parseAdjacencyMatrix l =
  foldr
    (\(valve, flowRate, connections) -> M.insert valve (flowRate, connections))
    M.empty
    $ map lineToAdjacency l

data SearchState = SearchState
  { position :: T.Text,
    time :: Int,
    openValves :: S.Set T.Text,
    totalPressureReleased :: Int
  }

startState :: SearchState
startState = SearchState {position = "AA", time = 1, openValves = S.empty, totalPressureReleased = 0}

totalFlow :: AdjacencyMatrix -> S.Set T.Text -> Int
totalFlow adjancencyMatrix openValves =
  S.fold
    ( \valve acc ->
        let (flow, _) = fromJust $ M.lookup valve adjancencyMatrix
         in acc + flow
    )
    0
    openValves

-- | parseLine
-- >>> parseTest parseLine "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
-- ("AA",0,["DD","II","BB"])
-- >>> parseTest parseLine "Valve HH has flow rate=22; tunnel leads to valve GG"
-- ("HH",22,["GG"])
part1 :: T.Text -> Int
part1 _ = 1

part2 :: T.Text -> Int
part2 _ = 1
