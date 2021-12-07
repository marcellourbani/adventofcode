#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List.Split (splitOn)

parse :: String -> [Int]
parse = map read . splitOn ","

-- >>> solve $ parse "16,1,2,0,4,2,7,1,2,14"
-- (37,[49,41,37,39,41,45,49,53,59,65,71,77,83,89,95,103,111])
solve :: [Int] -> Int
solve crabs = minimum fuelcosts
  where
    positions = [minimum crabs .. maximum crabs]
    fuelcost p = sum $ abs . (p -) <$> crabs
    fuelcosts = fuelcost <$> positions

main :: IO ()
main = readFile "input/day7.txt" >>= print . solve . parse
