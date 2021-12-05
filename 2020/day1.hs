#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List (tails)

-- >>> solve [1721,979,366,299,675,1456] -- (514579,241861950)
-- (514579,241861950)
solve :: [Int] -> (Int, Int)
solve l = (head couples, head triplets)
  where
    couples = [x * y | (x : ys) <- tails l, y <- ys, x + y == 2020]
    triplets = [x * y * z | (x : ys) <- tails l, (y : zs) <- tails ys, z <- zs, x + y + z == 2020]

main :: IO ()
main = readFile "input/day1.txt" >>= print . solve . fmap read . words
