module Day10 where

import Data.List (intercalate, unfoldr)
import Data.List.Split (chunksOf)
import qualified Data.Text as T

data Instruction = Noop | AddX Int deriving (Show)

parseCommand :: T.Text -> Instruction
parseCommand t =
  if t == "noop"
    then Noop
    else case T.stripPrefix "addx " t of
      Nothing -> error "invalid parse"
      Just str -> AddX (read $ T.unpack str)

timeToComplete :: (Num a) => Instruction -> a
timeToComplete Noop = 1
timeToComplete (AddX _) = 2

data ProgramState = ProgramState {value :: Int, pending :: [(Int, Instruction)]} deriving (Show)

initialProgramState :: ProgramState
initialProgramState = ProgramState {value = 1, pending = []}

tickDown :: [(Int, Instruction)] -> (Maybe Instruction, [(Int, Instruction)])
tickDown [] = (Nothing, [])
tickDown ((1, instr) : xs) = (Just instr, xs)
tickDown ((n, instr) : xs) = (Nothing, (n - 1, instr) : xs)

stepX :: Instruction -> Int -> Int
stepX Noop n = n
stepX (AddX dn) n = n + dn

step :: ProgramState -> Maybe Instruction -> ProgramState
step ps Nothing = ps
step ProgramState {value = x_value, pending = _pending} (Just instr) =
  (ProgramState {value = x_value, pending = _pending ++ [(timeToComplete instr, instr)]})

tick :: ProgramState -> ProgramState
tick ProgramState {value = x_value, pending = _pending} =
  let (result, _nextPending) = tickDown _pending
   in case result of
        Nothing -> ProgramState {value = x_value, pending = _nextPending}
        Just instr -> ProgramState {value = stepX instr x_value, pending = _nextPending}

-- I want an infinite sequence of items of a list or nothing
itemStream :: [a] -> [Maybe a]
itemStream = unfoldr f
  where
    f [] = Just (Nothing, [])
    f (x : xs) = Just (Just x, xs)

part1 :: T.Text -> Int
part1 =
  sum
    . map (\(n, s) -> (n + 1) * value s)
    . filter (\(n, _) -> (n + 1) `elem` [20, 60 .. 220])
    . take 300
    . zip [0 ..]
    . scanl (\ps instr -> tick $ step ps instr) initialProgramState
    . itemStream
    . map parseCommand
    . T.splitOn "\n"

part2 :: T.Text -> T.Text
part2 =
  T.pack
    . intercalate "\n"
    . map (map (\(n, s) -> let idx = n `mod` 40 in if value s `elem` [idx + 1, idx, idx - 1] then '#' else '.'))
    . chunksOf 40
    . take 240
    . zip [0 ..]
    . scanl (\ps instr -> tick $ step ps instr) initialProgramState
    . itemStream
    . map parseCommand
    . T.splitOn "\n"
