#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List (sort)
import Data.List.Split (splitOn)

parse :: String -> [((Int, Int), (Int, Int))]
parse s = parseline <$> lines s
  where
    parseline l = (parsepair a, parsepair b) where [a, b] = splitOn "," l
    parsepair p = (read a, read b) where [a, b] = splitOn "-" p

-- >>> solve $ parse "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"
-- (2,4)
solve :: [((Int, Int), (Int, Int))] -> (Int, Int)
solve l = (p1, p2)
  where
    p1 = length $ filter contained l
    p2 = length $ filter overlap l
    contained ((a, b), (c, d)) = (c >= a && d <= b) || (a >= c && b <= d)
    overlap ((a, b), (c, d)) = (c >= a && c <= b) || (a >= c && a <= d)

main :: IO ()
main = readFile "input/day04.txt" >>= print . solve . parse
