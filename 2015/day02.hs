#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List (sort)
import Data.List.Split (splitOn)

parse :: String -> [[Int]]
parse i = map read . splitOn "x" <$> lines i

-- >>> solve $ parse "2x3x4\n1x1x10"
-- (101,48)

solve :: [[Int]] -> (Int, Int)
solve l = (sum $ psize <$> l, sum $ ribbon <$> l)
  where
    faces [w, l, h] = [w * l, w * h, l * h]
    faces _ = []
    psize s = 2 * sum (faces s) + minimum (faces s)
    ribbon s = case sort s of
      [w, l, h] -> 2 * (w + l) + (w * l * h)
      _ -> 0

main :: IO ()
main = readFile "input/day02.txt" >>= print . solve . parse
