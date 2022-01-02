#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List.Split (splitOn)

parse :: String -> [Int]
parse = map read . splitOn ","

-- >>> solve $ parse "16,1,2,0,4,2,7,1,2,14"
-- (37,168)

solve :: [Int] -> (Int, Int)
solve crabs = (minimum fuelcosts, minimum fuelcosts2)
  where
    positions = [minimum crabs .. maximum crabs]
    fuelcost p = sum $ abs . (p -) <$> crabs
    fuelcosts = fuelcost <$> positions
    distcost a b = x * (x + 1) `div` 2 where x = abs (a - b) -- puzzle text asks for position, but verifies distance
    fuelcost2 p = sum $ distcost p <$> crabs
    fuelcosts2 = fuelcost2 <$> positions

main :: IO ()
main = readFile "input/day7.txt" >>= print . solve . parse