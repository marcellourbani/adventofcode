#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

{-# LANGUAGE PackageImports #-}

module Main where

import Data.List (sort)
import "containers" Data.Map.Strict as M

parse :: String -> [(Int, Int)]
parse s = readline <$> lines s
  where
    readline l = (a, b) where [a, b] = read <$> words l

part1 :: [(Int, Int)] -> Int
part1 l = sum $ abs <$> ds
  where
    lefts = sort $ fst <$> l
    rights = sort $ snd <$> l
    ds = uncurry (-) <$> zip lefts rights

part2 :: [(Int, Int)] -> Int
part2 l = sum $ score . fst <$> l
  where
    freqs = M.unionsWith (+) $ M.singleton . snd <$> l <*> [1]
    score n = n * M.findWithDefault 0 n freqs

-- >>> solve $ parse "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"
-- (11,31)

solve l = (p1, p2)
  where
    p1 = part1 l
    p2 = part2 l

main :: IO ()
main = readFile "input/day01.txt" >>= print . solve . parse
