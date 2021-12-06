#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List.Split (splitOn)

-- >>> parse "3,4,3,1,2"
-- [3,4,3,1,2]
parse :: String -> [Int]
parse = map read . splitOn ","

-- >>> solve $ parse "3,4,3,1,2"
-- 5934
solve :: [Int] -> Int
solve fishes = length last
  where
    cycle t
      | t == 0 = 6
      | otherwise = t - 1
    nextGen ts = map cycle ts ++ (8 <$ filter (== 0) ts)
    last = iterate nextGen fishes !! 80

main :: IO ()
main = readFile "input/day6.txt" >>= print . solve . parse
