#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (find)
import Data.List (tails)

parse :: String -> [[Int]]
parse s = map read . words <$> lines s

-- >>> solve $ parse "5 1 9 5\n7 5 3\n2 4 6 8"
-- >>> solve $ parse "5 9 2 8\n9 4 7 3\n3 8 6 5"
-- (18,7)
-- (18,9)

solve :: [[Int]] -> (Int, Int)
solve l = (sum $cs <$> l, sum $ go <$> l)
  where
    cs xs = maximum xs - minimum xs
    f a b = divMod (max a b) (min a b)
    go xs = case xs of
      y : ys -> case find ((== 0) . snd) $ f y <$> ys of
        Just (d, _) -> d
        Nothing -> go ys
      [] -> 0

main :: IO ()
main = readFile "input/day02.txt" >>= print . solve . parse
