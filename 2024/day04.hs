#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

{-# LANGUAGE PackageImports #-}

module Main where

import Linear.V2
import "containers" Data.Map.Strict qualified as M
import "containers" Data.Set qualified as S

type Input = M.Map (V2 Int) Char

dirVectors :: [V2 Int]
dirVectors = [V2 x y | x <- [-1 .. 1], y <- [-1 .. 1], (x, y) /= (0, 0)]

xVectors :: [V2 Int]
xVectors = [v | v@(V2 x y) <- dirVectors, x /= 0, y /= 0]

parse :: String -> Input
parse s = M.unions [M.singleton (V2 x y) c | (y, l) <- zip [0 ..] $ lines s, (x, c) <- zip [0 ..] l]

word :: Input -> Int -> V2 Int -> V2 Int -> String
word m len pos v
  | len <= 0 = ""
  | otherwise = M.findWithDefault '.' pos m : word m (len - 1) (pos + v) v

part1 :: Input -> Int
part1 l = length [d | d <- dirVectors, s <- starts, let w = word l 4 s d, w == "XMAS"]
  where
    starts = M.keys $ M.filter (== 'X') l

part2 :: Input -> Int
part2 l = S.size $ S.fromList xs
  where
    centers = M.keys $ M.filter (== 'A') l
    isMas c v = "MAS" == word l 3 (c - v) v
    xs = [c | d <- xVectors, c <- centers, isMas c d, p <- perpDir d, isMas c p]

perpDir :: V2 Int -> [V2 Int]
perpDir v = filter ((== 0) . dp v) dirVectors
  where
    dp (V2 x1 y1) (V2 x2 y2) = x1 * x2 + y1 * y2

-- >>> solve $ parse "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX"
-- (18,9)

solve l = (p1, p2)
  where
    p1 = part1 l
    p2 = part2 l

main :: IO ()
main = readFile "input/day04.txt" >>= print . solve . parse
