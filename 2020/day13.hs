#!/usr/bin/env stack
-- stack --resolver lts-10.2 script
module Main where

import           Data.List.Split

-- shamelessly copied from a python solution
modinv :: Integral a => a -> a -> a
modinv n m = mod inv m where
  (_,inv,_) = ee n m
  ee 0 b = (b,0,1)
  ee a b = (g,x - (div b a * y),y) where (g,y,x) = ee (mod b a) a


chinrem :: [(Int, Int)] -> Int
chinrem l = mod s mt where
  mt = product $ fst <$> l
  s = sum [ a * b * bi | (m,a)<-l,let b = div mt m,let bi = modinv b m ]

-- >>> solve "939\n7,13,x,x,59,x,31,19"
-- (295,1068781)

solve :: String -> (Int, Int)
solve s = (first ,second)
  where first = firstBus * (fisrtTime-arrival)
        second = chinrem constraints
        parseinput:: [String]->(Int,[Int],[(Int,Int)])
        parseinput (t:bs:_) = (read t, buses,constraints) where
          rawBuses = splitOn "," bs
          buses = read <$> filter (/="x") rawBuses
          constraints = [(id,v)| (i,x)<-zip [0..] rawBuses,let id = read x, let v = mod (id - i) id, x/="x"]
        (arrival,buses,constraints) = parseinput $ lines s
        nextPass start bus = if 0 == mod start bus then start else start + bus - mod start bus
        nextPasses = [(nextPass arrival bus,bus)|bus <-buses]
        (fisrtTime,firstBus) = minimum nextPasses

main :: IO ()
main = readFile "input/day13.txt" >>= print.solve
