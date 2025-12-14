{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Day17 where

import Data.List (uncons)
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (fromText, singleton, toLazyText)
import Debug.Trace (traceShow)
import GHC.IO (unsafePerformIO)
import Util (Coord2d, add2)

-- this a standard "simulate and then find a cycle" problem
-- I suppose there is no reason we can't have a Set to keep track of the rocks.

type Shape = Set Coord2d

longVertical :: Shape
longVertical = Set.fromList [(0, 0), (1, 0), (2, 0), (3, 0)]

cross :: Shape
cross = Set.fromList [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]

lbar :: Shape
lbar = Set.fromList [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]

longHorizontal :: Shape
longHorizontal = Set.fromList [(0, 0), (0, 1), (0, 2), (0, 3)]

square :: Shape
square = Set.fromList [(0, 0), (1, 0), (0, 1), (1, 1)]

shapeList :: [Shape]
shapeList = cycle [longVertical, cross, lbar, longHorizontal, square]

-- int is maxY
-- technically this grid works backwards (y = 0 is the bottom, y = 30 is 30 units up, etc)
data Chamber = Chamber Int (Set Coord2d) (Maybe Shape) deriving (Show)

-- maxY only gets updated when a shape ends
placeShape :: Shape -> Chamber -> Chamber
placeShape shape (Chamber maxY rocks _) = Chamber maxY rocks (Just (Set.map (add2 (2, 3 + maxY)) shape))

data Gust = LeftGust | RightGust deriving (Show)

type ChamberState = (Chamber, [Gust], [Shape])

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
        [maxY, maxY - 1 .. 0]
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
step (Chamber maxY rocks Nothing, gusts, shapes) = (placeShape (head shapes) (Chamber maxY rocks Nothing), gusts, tail shapes)
step (Chamber maxY rocks (Just shape), gusts, shapes) =
  case moveDown rocks gustedShape of
    Nothing -> step (Chamber newMaxY (Set.union gustedShape rocks) Nothing, restGusts, shapes)
    next -> step (Chamber maxY rocks next, restGusts, shapes)
  where
    -- jet, then push down. we will recurse here so step()'s results can just be the next
    (gust, restGusts) = fromJust $ uncons gusts
    gustedShape = applyGust rocks gust shape
    newMaxY = foldr (\c acc -> max acc (snd c)) maxY gustedShape + 1

parseGusts :: T.Text -> [Gust]
parseGusts t =
  map
    ( \case
        '>' -> RightGust
        '<' -> LeftGust
        _ -> error "impossible"
    )
    $ T.unpack t

part1 :: T.Text -> Int
part1 t = case iterate step (Chamber 0 Set.empty Nothing, cycle (parseGusts t), shapeList) !! 2022 of
  (Chamber maxY _ _, _, _) -> maxY

part2 :: T.Text -> Int
part2 _ = 1
