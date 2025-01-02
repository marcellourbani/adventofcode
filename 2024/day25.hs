#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

module Main where

import Data.Either (lefts, rights)
import Data.List (transpose)
import Data.List.Split (splitOn)

newtype Lock = Lock [Int] deriving (Show)

newtype Key = Key [Int] deriving (Show)

type Input = [Either Lock Key]

parse :: String -> Input
parse s = parseKey <$> splitOn "\n\n" s
  where
    ch a = length $ filter (== '#') a
    parseKey a = case head a of -- transpose $ lines a
      '#' -> Left $ Lock $ ch <$> transpose (tail $ lines a)
      _ -> Right $ Key $ ch <$> transpose (tail $ reverse $ lines a)

fits :: Lock -> Key -> Bool
fits (Lock ls) (Key ks) = all ((<= 5) . uncurry (+)) $ zip ls ks

solve :: Input -> Int
solve l = length [(k, l) | k <- keys, l <- locks, fits k l]
  where
    combs = [(k, l) | k <- keys, l <- locks, fits k l]
    keys = lefts l
    locks = rights l

-- >>> solve $ parse "#####\n.####\n.####\n.####\n.#.#.\n.#...\n.....\n\n#####\n##.##\n.#.##\n...##\n...#.\n...#.\n.....\n\n.....\n#....\n#....\n#...#\n#.#.#\n#.###\n#####\n\n.....\n.....\n#.#..\n###..\n###.#\n###.#\n#####\n\n.....\n.....\n.....\n#....\n#.#..\n#.#.#\n#####"
-- 3

main :: IO ()
main = readFile "input/day25.txt" >>= print . solve . parse
