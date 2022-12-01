#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where
import           Data.List       (sort)
import           Data.List.Split (splitOn)

parse::String->[[Int]]
parse s = map read <$> items
  where
    items = splitOn [[]] $ lines s

-- >>> solve $ parse "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"
-- (24000,45000)
solve ::[[Int]] -> (Int,Int)
solve l = (maximum elves, sum $ take 3 $ reverse $ sort elves)
  where
    elves = sum <$> l

main :: IO ()
main = readFile "input/day1.txt" >>= print . solve . parse
