#!/usr/bin/env stack
-- stack --resolver lts-10.2 script
module Main where

import           Data.List.Split
-- >>> solve "939\n7,13,x,x,59,x,31,19"
-- (295,59)

-- solve :: [Char] -> (Int, Int)
solve s = (first ,second)
  where first = firstBus * (fisrtTime-arrival)
        second = firstBus
        parseinput:: [String]->(Int,[Int],[(Int,Int)])
        parseinput (t:bs:_) = (read t, buses,constraints) where
          rawBuses = splitOn "," bs
          buses = read <$> filter (/="x") rawBuses
          constraints = [(read x,i)| (i,x)<-zip [0..] rawBuses , x/="x"]        
        (arrival,buses,constraints) = parseinput $ lines s
        nextPass start bus = if 0 == mod start bus then start else start + bus - mod start bus
        nextPasses = [(nextPass arrival bus,bus)|bus <-buses]
        (fisrtTime,firstBus) = minimum nextPasses

main :: IO ()
main = readFile "input/day13.txt" >>= print.solve
