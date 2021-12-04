#!/usr/bin/env stack
-- stack --resolver lts-10.2 script

module Main where

import Data.List (tails)

-- >>> solve [199,200,208,210,200,207,240,269,260,263] -- 7
-- (7,5)
solve :: [Int] -> (Int, Int)
solve l = (length $couples l, length $couples sums)
  where
    couples l' = [(cur, prev) | (prev, cur) <- zip l' $ tail l', cur > prev]
    sums = [a + b + c | (a, b, c) <- zip3 l (tail l) (tail $tail l)]

main :: IO ()
main = readFile "input/day1.txt" >>= print . solve . fmap read . words
