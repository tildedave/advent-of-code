{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import Data.List (transpose)
import qualified Data.Text as T

applyOperation :: [T.Text] -> Int
applyOperation l =
  let (ops, op) = (init l, last l)
   in ( case op of
          "+" -> sum
          "*" -> product
          _ -> error "invalid"
      )
        $ (read . T.unpack <$> ops)

part1 :: T.Text -> Int
part1 t =
  sum $ map applyOperation $ transpose $ filter (not . T.null) . T.split (== ' ') <$> T.splitOn "\n" t

part2 :: T.Text -> Int
part2 _ = 1
