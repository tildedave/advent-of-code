{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Day17 where

import Data.List (uncons)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (fromText, singleton, toLazyText)
import Util (Coord2d, add2)

-- this a standard "simulate and then find a cycle" problem
-- I suppose there is no reason we can't have a Set to keep track of the rocks.

type Shape = Set Coord2d

longHorizontal :: Shape
longHorizontal = Set.fromList [(0, 0), (1, 0), (2, 0), (3, 0)]

cross :: Shape
cross = Set.fromList [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]

lbar :: Shape
lbar = Set.fromList [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]

longVertical :: Shape
longVertical = Set.fromList [(0, 0), (0, 1), (0, 2), (0, 3)]

square :: Shape
square = Set.fromList [(0, 0), (1, 0), (0, 1), (1, 1)]

-- int is maxY
-- technically this grid works backwards (y = 0 is the bottom, y = 30 is 30 units up, etc)
data Chamber = Chamber Int (Set Coord2d) (Maybe Shape) deriving (Show)

prune :: Chamber -> Chamber
prune (Chamber maxY rocks shape) =
  let filterY = max (maxY - 20) 0
   in Chamber maxY (Set.filter ((>= filterY) . snd) rocks) shape

-- maxY only gets updated when a shape ends
placeShape :: Shape -> Chamber -> Chamber
placeShape shape (Chamber maxY rocks _) = Chamber maxY rocks (Just (Set.map (add2 (2, 3 + maxY)) shape))

data Gust = LeftGust | RightGust deriving (Show)

type ChamberState = (Chamber, [(Int, Gust)], [(Int, Shape)])

chamberToString :: Chamber -> T.Text
chamberToString (Chamber maxY rocks shape) =
  toStrict $
    toLazyText $
      foldr
        ( \y b ->
            singleton '\n'
              <> foldr
                ( \x b' ->
                    singleton
                      ( if
                          | (x, y) `elem` rocks -> '#'
                          | (x, y) `elem` shapeCoords -> '@'
                          | otherwise -> '.'
                      )
                      <> b'
                )
                b
                [0 .. 6]
        )
        (fromText "")
        [maxY + 10, maxY + 9 .. 0]
  where
    shapeCoords = fromMaybe Set.empty shape

applyGust :: Set Coord2d -> Gust -> Shape -> Shape
applyGust rocks gust shape =
  if all (\c -> notElem c rocks && fst c >= 0 && fst c < 7) nextShape
    then
      nextShape
    else shape
  where
    delta = case gust of
      LeftGust -> (-1, 0)
      RightGust -> (1, 0)
    nextShape = Set.map (add2 delta) shape

moveDown :: Set Coord2d -> Shape -> Maybe Shape
moveDown rocks shape =
  if all (\c -> notElem c rocks && snd c >= 0) nextShape
    then
      Just nextShape
    else
      Nothing
  where
    nextShape = Set.map (add2 (0, -1)) shape

step :: ChamberState -> ChamberState
step (Chamber maxY rocks Nothing, gusts, shapes) = (placeShape (snd (head shapes)) (Chamber maxY rocks Nothing), gusts, tail shapes)
step (Chamber maxY rocks (Just shape), gusts, shapes) =
  case moveDown rocks gustedShape of
    Nothing -> step (Chamber newMaxY (Set.union gustedShape rocks) Nothing, restGusts, shapes)
    next -> step (Chamber maxY rocks next, restGusts, shapes)
  where
    -- jet, then push down. we will recurse here so step()'s results can just be the next
    (gust, restGusts) = fromJust $ uncons gusts
    gustedShape = applyGust rocks (snd gust) shape
    newMaxY = foldr (\c acc -> max acc (snd c + 1)) maxY gustedShape

parseGusts :: T.Text -> [Gust]
parseGusts t =
  map
    ( \case
        '>' -> RightGust
        '<' -> LeftGust
        _ -> error "impossible"
    )
    $ T.unpack t

rockSeqList :: T.Text -> [ChamberState]
rockSeqList t =
  tail -- discard first state since it's just a "placement" of the first shape
    ( iterate
        step
        ( Chamber 0 Set.empty Nothing,
          cycle (zip [0 ..] (parseGusts t)),
          cycle (zip [0 ..] [longHorizontal, cross, lbar, longVertical, square])
        )
    )

maxRocks :: ChamberState -> Int
maxRocks (Chamber maxY _ _, _, _) = maxY

-- changing this results in a still incorrect answer
chamberStateToHash :: ChamberState -> ([[Char]], Int, Int)
chamberStateToHash (Chamber maxY rocks _, (gn, _) : _, (sn, _) : _) =
  ( map
      ( \y ->
          map
            (\x -> if (x, y) `Set.member` rocks then '#' else '.')
            [0 .. 6]
      )
      (takeWhile (>= 0) [maxY, maxY - 1 .. maxY - 20]),
    gn,
    sn
  )
chamberStateToHash _ = error "impossible"

--   )
-- chamberStateToHash :: ChamberState -> (Set Coord2d, Int, Int)
-- chamberStateToHash (Chamber _ rocks _, (gn, _) : _, (sn, _) : _) =
--   (Set.map (\(x, y) -> (x, y - minY)) rocks, gn, sn)
--   where
--     minY = foldr (min . snd) maxBound rocks
-- chamberStateToHash _ = error "impossible"

-- 1552 is the first deviation
towerHeight :: Int -> T.Text -> Int
towerHeight goal' t =
  let loop ((n, chamberState) : xs) m goal =
        if n == goal
          then
            maxRocks chamberState
          else
            let h = chamberStateToHash chamberState
             in case Map.lookup h m of
                  Nothing -> loop xs (Map.insert h (n, maxRocks chamberState) m) goal
                  Just (prevN, prevRocks) ->
                    -- prevN ---> n is a cycle
                    let currentHeight = maxRocks chamberState
                        rocksPerCycle = currentHeight - prevRocks
                        cycleLength = n - prevN
                        numFullCycles = (goal - prevN) `div` cycleLength
                        left = (goal - prevN) `rem` cycleLength
                     in maxRocks
                          (chamberSeq !! (left + prevN))
                          + numFullCycles * rocksPerCycle
      loop _ _ _ = error "invalid"
   in loop (zip [0 ..] chamberSeq) Map.empty goal'
  where
    chamberSeq = rockSeqList t

-- | Examples
-- >>> s = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
-- >>> part1 s
-- 3068
-- >>> part2 s
-- 1514285714288
part1 :: T.Text -> Int
part1 = towerHeight 2022

part2 :: T.Text -> Int
part2 = towerHeight 1000000000000
