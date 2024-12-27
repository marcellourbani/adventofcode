#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

{-# LANGUAGE PackageImports #-}

module Main where

import Data.Bits (xor)
import "containers" Data.Map.Strict qualified as M

type Input = [Int]

parse :: String -> Input
parse s = read <$> lines s

nextSecret :: Int -> Int
nextSecret s = prune $ 2048 * p2 `xor` p2
  where
    prune = (`mod` 16777216)
    p1 = prune $ (s * 64) `xor` s
    p2 = prune $ div p1 32 `xor` p1

part1 :: [[Int]] -> Int
part1 l = sum $ (!! 2000) <$> l

part2 :: [[Int]] -> Int
part2 l = maximum counts
  where
    digits a = read . (: []) . last . show <$> a
    deltas a = zip (tail a) $ uncurry (-) <$> zip (tail a) a
    firsts a m = case a of
      (_, s1) : (_, s2) : (_, s3) : (v, s4) : as
        | M.member (s1, s2, s3, s4) m -> firsts (tail a) m
        | otherwise -> firsts (tail a) $ M.insert (s1, s2, s3, s4) v m
      _ -> m
    counts = M.unionsWith (+) $ firsts . deltas . digits <$> l <*> [M.empty]

-- >>> solve $ parse "1\n10\n100\n2024"
-- >>> solve $ parse "1\n2\n3\n2024"
-- (37327623,24)
-- (37990510,23)

solve :: Input -> (Int, Int)
solve l = (p1, p2)
  where
    twoks s = take 2001 $ iterate nextSecret s
    seqs = twoks <$> l
    p1 = part1 seqs
    p2 = part2 seqs

main :: IO ()
main = readFile "input/day22.txt" >>= print . solve . parse
