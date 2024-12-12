#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

{-# LANGUAGE PackageImports #-}

module Main where

import "containers" Data.Map.Strict qualified as M

type Input = [Int]

parse :: String -> Input
parse s = read <$> words s

evolve :: Int -> [Int]
evolve !i
  | i == 0 = [1]
  | even li = [read $ take mi s, read $ drop mi s]
  | otherwise = [i * 2024]
  where
    !s = show i
    !li = length s
    !mi = div li 2

blink :: [Int] -> [Int]
blink !l = l >>= evolve

part1 :: Input -> Int
part1 i = length $ iterate blink i !! 25

part2 :: Input -> Int
part2 i = sum $ iterate blinkf initial !! 75
  where
    initial = M.unionsWith (+) $ freqs $ (,1) <$> i
    freqs l = uncurry M.singleton <$> l
    evolvef (n, f) = M.unionsWith (+) $ freqs $ (,f) <$> evolve n
    blinkf m = M.unionsWith (+) $ evolvef <$> M.toList m

-- >>> solve $ parse "125 17"
-- (55312,65601038650482)

solve :: Input -> (Int, Int)
solve i = (p1, p2)
  where
    p1 = part1 i
    p2 = part2 i

main :: IO ()
main = readFile "input/day11.txt" >>= print . solve . parse
