module Day18 where

import Data.List (tails)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import Text.Megaparsec.Byte (string)
import Text.Megaparsec.Char.Lexer qualified as L
import Util (allSame, allUnique, fst3, parseOrDie, snd3, thd3)

-- (1,1,1) (the point) has faces
-- for dx = 0, 1, dy = 0, 1, dz = 0, 1

type Point3d = (Int, Int, Int)

vertices :: Point3d -> [Point3d]
vertices (x, y, z) = [(x + dx, y + dy, z + dz) | dx <- [0 .. 1], dy <- [0 .. 1], dz <- [0 .. 1]]

-- distinct :: (Equal a) => [a] -> Bool
-- distinct

inSamePlane :: [Point3d] -> Bool
inSamePlane pl = allUnique pl && (allSame (map fst3 pl) || allSame (map snd3 pl) || allSame (map thd3 pl))

-- | inSamePlace
-- >>> inSamePlane [(1,1,2),(2,1,2),(2,2,2),(1,2,2)]
-- True
-- >>> inSamePlane [(1,1,3),(2,1,2),(2,2,2),(1,2,2)]
-- False
faces :: Point3d -> [(Point3d, Point3d, Point3d, Point3d)]
faces pt =
  [ (p1, p2, p3, p4)
  | (p1 : r1) <- tails vl,
    (p2 : r2) <- tails r1,
    (p3 : r3) <- tails r2,
    p4 <- r3,
    inSamePlane [p1, p2, p3, p4]
  ]
  where
    vl = vertices pt

type Parser = Parsec Void T.Text

parsePoint :: Parser (Point3d)
parsePoint = (,,) <$> L.decimal <* string "," <*> L.decimal <* string "," <*> L.decimal

-- | Faces
-- >>> length (faces (1,1,1))
-- 6
part1 :: T.Text -> Int
part1 t =
  let allFaces = concatMap (faces . parseOrDie parsePoint) $ T.splitOn "\n" t
      faceCount = M.fromListWith (+) [(f, 1 :: Integer) | f <- allFaces]
   in M.size $ M.filter (== 1) faceCount

-- | part1
-- >>> part1 "2,2,2\n1,2,2\n3,2,2\n2,1,2\n2,3,2\n2,2,1\n2,2,3\n2,2,4\n2,2,6\n1,2,5\n3,2,5\n2,1,5\n2,3,5"
-- 64
part2 :: T.Text -> Int
part2 _ = 1
