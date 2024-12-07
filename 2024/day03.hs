#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

module Main where

import Text.Regex.TDFA

parse :: String -> [(Int, Int)]
parse s = lines s >>= readline
  where
    readline l = read . drop 3 <$> getAllTextMatches (l =~ "mul\\([0-9]+,[0-9]+\\)")

part1 :: [(Int, Int)] -> Int
part1 l = sum $ uncurry (*) <$> l

part2 :: [(Int, Int)] -> Int
part2 l = 0

-- >>> solve $ parse "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
-- (161,0)

solve l = (p1, p2)
  where
    p1 = part1 l
    p2 = part2 l

main :: IO ()
main = readFile "input/day03.txt" >>= print . solve . parse
