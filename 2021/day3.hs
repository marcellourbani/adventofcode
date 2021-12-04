#!/usr/bin/env stack
-- stack --resolver lts-10.2 script

module Main where

import Data.List (tails, transpose)

-- >>> mostFrequent "00100"
-- 0

mostFrequent :: String -> Int
mostFrequent l
  | count > mid = 1
  | otherwise = 0
  where
    count = sum $ read . (: []) <$> l
    mid = length l `div` 2

-- >>> toDecimal [1,0,1,1,0]
-- 22
toDecimal :: [Int] -> Int
toDecimal l = sum [2 ^ exp * x | (x, exp) <- zip l [max, max -1 .. 0]]
  where
    max = length l -1

-- >>> solve [ "00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]
-- 198

solve :: [[Char]] -> Int
solve l = gamma * epsilon
  where
    bits = mostFrequent <$> transpose l
    gamma = toDecimal bits
    epsilon = 2 ^ length bits - gamma - 1

main :: IO ()
main = readFile "input/day3.txt" >>= print . solve . words
