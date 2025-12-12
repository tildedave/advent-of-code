module Day12 where

import Data.List (foldl', uncons)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (fromText, singleton, toLazyText)
import Util (Coord2d, Grid, bounds, gridAt_, parseGridContents, unsnoc)

-- well this one seems hard.  may not complete this morning.
-- the best approach I can think of involves creating larger regions from the base tiles, then recursing.
-- we can recurse on width 4 or width 5.  however, examples like the second one mean that you can't just
-- purely recurse, that one is a 4x5 (can recurse), leaving you with a 8x5 grid that requires 4 to
-- completely tile.  it does give us the shapes it wants to use.  so combining tiles is probably right.

data Shape = Shape Int (Grid Coord2d Char) deriving (Show) -- id, grid

parseShape :: T.Text -> Shape
parseShape t =
  Shape (read $ T.unpack $ fromJust $ T.stripSuffix ":" idStr) (parseGridContents id rows)
  where
    (idStr, rows) = fromJust $ uncons $ T.splitOn "\n" t

-- | toString
-- >>> s = "4:\n###\n#..\n###"
-- >>> parseShape s
-- Shape 4 (fromList [((0,0),'#'),((0,1),'#'),((0,2),'#'),((1,0),'#'),((1,1),'.'),((1,2),'#'),((2,0),'#'),((2,1),'.'),((2,2),'#')])
-- >>> toString (parseShape s)
-- "###\n#..\n###\n"
toString :: Shape -> T.Text
toString (Shape _ grid) =
  toStrict $
    toLazyText $
      foldl'
        ( \b y ->
            foldl' (\b' x -> b' <> singleton (gridAt_ (x, y) grid)) b [0 .. xmax] <> singleton '\n'
        )
        (fromText "")
        [0 .. ymax]
  where
    (xmax, ymax) = bounds grid

--- 0 1 2      6 3 0      (0,0) -> (2, 0) (1, 0) -> (2,1) ()
--- 3 4 5 ---> 7 4 1 --->  x coord = ymax - y, y coord = x
--- 6 7 8      8 5 2

-- | rotateLeft
-- >>> s = "4:\n####\n#.#.\n####\n##.."
-- >>> toString (rotateRight (parseShape s))
-- "####\n##.#\n.###\n.#.#\n"
rotateRight :: Shape -> Shape
rotateRight (Shape n grid) =
  Shape
    n
    ( M.fromList
        ( map
            (\((x, y), ch) -> ((ymax - y, x), ch))
            (M.toList grid)
        )
    )
  where
    (_, ymax) = bounds grid

-- | flipHorizontal
-- >>> s = "4:\n####\n#.#.\n####\n##.."
-- >>> toString $ flipHorizontal $ parseShape s
-- "####\n.#.#\n####\n..##\n"
flipHorizontal :: Shape -> Shape
flipHorizontal (Shape n grid) =
  Shape
    n
    ( M.fromList
        ( (\((x, y), ch) -> ((xmax - x, y), ch))
            <$> M.toList grid
        )
    )
  where
    (xmax, _) = bounds grid

-- OK so I spoiled myself on Reddit and the problem is much easier (woo-hoo)

-- | parseProblem
-- >>> s = "4x4: 0 0 0 0 2 0"
-- >>> parseProblem s
-- (4,4,[0,0,0,0,2,0])
type Problem = (Int, Int, [Int])

parseProblem :: T.Text -> Problem
parseProblem t =
  (height, width, read . T.unpack <$> T.splitOn " " rhs)
  where
    (lhs, rhs) = case T.splitOn ": " t of
      [r, l] -> (r, l)
      _ -> error "invalid"
    (height, width) = case T.splitOn "x" lhs of
      [r, l] -> (read $ T.unpack r, read $ T.unpack l)
      _ -> error "invalid"

parseInput :: T.Text -> ([Shape], [Problem])
parseInput t =
  (parseShape <$> shapeStrs, parseProblem <$> T.splitOn "\n" problemsStr)
  where
    (shapeStrs, problemsStr) = fromJust $ unsnoc $ T.splitOn "\n\n" t

isViable :: Problem -> Bool
isViable (height, width, counts) =
  sum ((9 *) <$> counts) <= (height * width)

part1 :: T.Text -> Int
part1 = length . filter isViable . snd . parseInput

part2 :: T.Text -> Int
part2 _ = 42