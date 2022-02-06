#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List (tails)

parse :: String -> [Int]
parse s = read . (: "") <$> s

-- >>> solve . parse <$> ["1122","1111","1234","91212129"]
-- >>> solve . parse <$> ["1212","1221","123425","123123","12131415"]
-- [(3,0),(4,4),(0,0),(9,6)]
-- [(0,6),(3,0),(0,4),(0,12),(0,4)]
solve :: [Int] -> (Int, Int)
solve l = (go $ l <> [head l], go2 (take le l) (drop le l))
  where
    le = div (length l) 2
    go l = case l of
      a : b : _
        | a == b -> a + go (tail l)
        | otherwise -> go (tail l)
      _ -> 0
    go2 l1 l2 = case (l1, l2) of
      (x1 : xs, y1 : ys)
        | x1 == y1 -> x1 * 2 + go2 xs ys
        | otherwise -> go2 xs ys
      _ -> 0

main :: IO ()
main = readFile "input/day01.txt" >>= print . solve . parse
