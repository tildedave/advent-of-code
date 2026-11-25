{-# LANGUAGE LambdaCase #-}

module Day8 where

import Data.Heap (MinPrioHeap)
import Data.Heap qualified as H
import Data.List (sortBy)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Ord (Down (Down), comparing)
import Data.Text qualified as T

-- this is a clustering algorithm w/ a union find flavor
-- we don't need a real disjoint set data structure, map will be fine

type Point3d = (Int, Int, Int)

parsePoint :: T.Text -> Point3d
parsePoint t = case read . T.unpack <$> T.splitOn "," t of
  [x, y, z] -> (x, y, z)
  _ -> error "invalid"

-- no reason to take the square root
distance :: Point3d -> Point3d -> Int
distance (x1, y1, z1) (x2, y2, z2) = square (x1 - x2) + square (y1 - y2) + square (z1 - z2)
  where
    square n = n * n

closestPairs :: [Point3d] -> MinPrioHeap Int (Point3d, Point3d)
closestPairs l =
  foldr
    (\(p1, p2) h -> H.insert (distance p1 p2, (p1, p2)) h)
    H.empty
    [(x, y) | x <- l, y <- l, x /= y, x < y]

-- | Disjoint Set tests
-- >>> s :: DisjointSet Int = emptyDisjointSet
-- >>> fst $ countComponents (union 1 2 (makeSet 2 (makeSet 1 s)))
-- fromList [(2,2)]
-- >>> fst $ countComponents (makeSet 2 (makeSet 1 s))
-- fromList [(1,1),(2,1)]
type DisjointSet a = (M.Map a Int, M.Map a a)

emptyDisjointSet :: DisjointSet a
emptyDisjointSet = (M.empty, M.empty)

makeSet :: (Ord a) => a -> DisjointSet a -> DisjointSet a
makeSet p (m, pm) = (M.insert p 0 m, M.insert p p pm)

link :: (Ord a) => a -> a -> DisjointSet a -> DisjointSet a
link x y (m, pm)
  | rankX > rankY = (m, M.insert y x pm)
  | rankX == rankY = (M.insert y (rankY + 1) m, M.insert x y pm)
  | otherwise = (m, M.insert x y pm)
  where
    rankX = fromJust $ M.lookup x m
    rankY = fromJust $ M.lookup y m

findSet :: (Ord a, Eq a) => a -> DisjointSet a -> (a, DisjointSet a)
findSet x ds =
  if x == p
    then
      (x, ds)
    else
      let (p', (m', pm')) = findSet p ds
       in (p', (m', M.insert x p' pm'))
  where
    (_, pm) = ds
    p = fromJust (M.lookup x pm)

union :: (Ord a) => a -> a -> DisjointSet a -> DisjointSet a
union x y ds =
  let (px, ds1) = findSet x ds
      (py, ds2) = findSet y ds1
   in link px py ds2

countComponents :: (Ord a) => DisjointSet a -> (M.Map a Int, DisjointSet a)
countComponents (m, pm) =
  foldr
    ( \k (s', ds) ->
        let (p, ds') = findSet k ds
         in ( M.alter
                ( \case
                    Nothing -> Just 1
                    Just q -> Just (q + 1)
                )
                p
                s',
              ds'
            )
    )
    (M.empty, (m, pm))
    (M.keys pm)

-- | Given examples
-- >>> s = "162,817,812\n57,618,57\n906,360,560\n592,479,940\n352,342,300\n466,668,158\n542,29,236\n431,825,988\n739,650,466\n52,470,668\n216,146,977\n819,987,18\n117,168,530\n805,96,715\n346,949,466\n970,615,88\n941,993,340\n862,61,35\n984,92,344\n425,690,689"
-- >>> topThreeCircuitSizes 10 s
-- 40
-- >>> part2 s
-- 25272
topThreeCircuitSizes :: Int -> T.Text -> Int
topThreeCircuitSizes n t =
  product $
    take 3 $
      sortBy (comparing Down) $
        map snd $
          M.toList $
            fst $
              countComponents $
                foldr
                  (\(_, (p1, p2)) ds -> union p1 p2 ds)
                  (foldr makeSet emptyDisjointSet points)
                  (H.take n (closestPairs points))
  where
    points = parsePoint <$> T.splitOn "\n" t

part1 :: T.Text -> Int
part1 = topThreeCircuitSizes 1000

connect :: MinPrioHeap Int (Point3d, Point3d) -> Int -> DisjointSet Point3d -> Int
connect h numLeft ds =
  case H.viewHead h of
    Nothing -> error "ran out of points to connect"
    Just (_, (p1, p2)) ->
      let (p1p, _) = findSet p1 ds
          (p2p, _) = findSet p2 ds
          nextds = union p1 p2 ds
          nextNumLeft = numLeft - (if p1p /= p2p then 1 else 0)
       in if nextNumLeft == 1
            then case (p1, p2) of
              ((x1, _, _), (x2, _, _)) -> x1 * x2
            else
              connect (H.drop 1 h) nextNumLeft nextds

part2 :: T.Text -> Int
part2 t =
  connect
    (closestPairs points)
    (length points)
    (foldr makeSet emptyDisjointSet points)
  where
    points = parsePoint <$> T.splitOn "\n" t
