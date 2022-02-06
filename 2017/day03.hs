#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (find)
import Data.List (tails)
import GHC.Float (int2Double)

parse :: String -> Int
parse = read

-- >>> solve $parse "1024"
-- (31,0)

solve :: Int -> (Int, Int)
solve l = (s, 0)
  where
    s = go 1
    go n
      | l == 1 = 0
      | n ^ 2 < l = go (n + 2)
      | otherwise = c + toc
      where
        r = l - (n -2) ^ 2
        c = div n 2
        toc = abs (c - r `mod` (n -1))

main :: IO ()
main = readFile "input/day03.txt" >>= print . solve . parse
