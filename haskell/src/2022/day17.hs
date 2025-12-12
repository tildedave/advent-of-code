{-# LANGUAGE LambdaCase #-}

module Day17 where

import Data.List (uncons)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Util (Coord2d, add2)

-- this a standard "simulate and then find a cycle" problem
-- I suppose there is no reason we can't have a Set to keep track of the rocks.

type Shape = Set Coord2d

plus :: Shape
plus = Set.fromList [(0, 0), (1, 0), (2, 0), (3, 0)]

cross :: Shape
cross = Set.fromList [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]

lbar :: Shape
lbar = Set.fromList [(0, 2), (1, 2), (2, 2), (2, 0), (2, 1)]

long :: Shape
long = Set.fromList [(0, 0), (0, 1), (0, 2), (0, 3)]

square :: Shape
square = Set.fromList [(0, 0), (1, 0), (0, 1), (1, 1)]

shapeList :: [Shape]
shapeList = cycle [plus, cross, lbar, long, square]

-- int is maxY
-- technically this grid works backwards (y = 0 is the bottom, y = 30 is 30 units up, etc)
data Chamber = Chamber Int (Set Coord2d) (Maybe Shape)

-- maxY only gets updated when a shape ends
placeShape :: Shape -> Chamber -> Chamber
placeShape shape (Chamber maxY rocks _) = Chamber maxY rocks (Just (Set.map (add2 (2, 3 + maxY)) shape))

data Gust = LeftGust | RightGust

type ChamberState = (Chamber, [Gust], [Shape])

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
  if any (`elem` rocks) nextShape
    then
      Nothing
    else
      Just nextShape
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
    newMaxY = foldr (\c acc -> max acc (snd c)) maxY gustedShape

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
part1 t = case iterate step (Chamber 0 (Set.fromList $ (,-1) <$> [0 .. 6]) Nothing, cycle (parseGusts t), shapeList) !! 2022 of
  (Chamber maxY _ _, _, _) -> maxY

part2 :: T.Text -> Int
part2 _ = 1