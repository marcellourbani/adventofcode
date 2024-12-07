#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

module Main where

import Data.List (filter)

type Input = [[Int]]

parse :: String -> Input
parse s = map read . words <$> lines s

safe :: [Int] -> Bool
safe s = case s of
  [] -> True
  [a, b] -> inrange a b
  a : b : c : xs -> inrange a b && samedir a b c && safe (b : c : xs)
  where
    inrange a b = abs (a - b) >= 1 && abs (a - b) <= 3
    samedir a b c = a > b && b > c || a < b && b < c

safe2 :: [Int] -> Bool
safe2 s = any safe sl
  where
    sl = s : [take i s <> drop (i + 1) s | i <- [0 .. length s - 1]]

part1 :: Input -> Int
part1 l = length $ filter safe l

part2 :: Input -> Int
part2 l = length $ filter safe2 l

-- >>> solve $ parse "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9"
-- (2,4)

solve l = (p1, p2)
  where
    p1 = part1 l
    p2 = part2 l

main :: IO ()
main = readFile "input/day02.txt" >>= print . solve . parse
