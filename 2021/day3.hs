#!/usr/bin/env stack
-- stack --resolver lts-10.2 script

module Main where

import Data.List (tails, transpose)

-- >>> mostFrequent [0,0,1,0,0,1,1,1]
-- 1

mostFrequent :: [Int] -> Int
mostFrequent l
  | zeros > ones = 0
  | otherwise = 1
  where
    ones = sum l
    zeros = length l - ones

-- >>> toDecimal [1,0,1,1,0]
-- 22
toDecimal :: [Int] -> Int
toDecimal l = sum [2 ^ exp * x | (x, exp) <- zip l [max, max -1 .. 0]]
  where
    max = length l -1

-- >>> solve [ "00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]
-- (198,230)

solve :: [[Char]] -> (Int, Int)
solve l = (gamma * epsilon, co2rate * oxyrate)
  where
    li = map (read . (: [])) <$> l
    bits = mostFrequent <$> transpose li
    gamma = toDecimal bits
    epsilon = toDecimal $ invert bits
    invert x = (1 -) <$> x
    oxyrate = toDecimal $ myfilter False 0 li
    co2rate = toDecimal $ myfilter True 0 li
    myfilter inverted n l = case l of
      [] -> []
      [x] -> x
      _ -> myfilter inverted (n + 1) filtered
      where
        rawcriteria = mostFrequent $ (!! n) <$> l
        criteria = (if inverted then (1 -) else id) rawcriteria
        filtered = [x | x <- l, x !! n == criteria]

main :: IO ()
main = readFile "input/day3.txt" >>= print . solve . words
