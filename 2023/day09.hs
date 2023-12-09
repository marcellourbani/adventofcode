#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List (tails)

parse :: String -> [[Int]]
parse s = map read . words <$> lines s

-- >>> solve $ parse "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45"
-- (114,2)
solve :: [[Int]] -> (Int, Int)
solve m = (p1, p2)
  where
    p1 = sum $ predictions <$> m
    p2 = sum $ revPredictions <$> m

    revPredictions'' = revPred <$> (reverse . map head . deriveall <$> m)
    revPredictions l = revPred $ reverse $ head <$> deriveall l

    revPred l = case l of
      a : b : vs -> revPred (b - a : vs)
      [a] -> a
      _ -> 0

    predictions l = sum (last <$> deriveall l)
    deriveall l
      | all (== 0) l = []
      | otherwise = l : deriveall (derive l)

    derive l = case l of
      a : b : vs -> (b - a) : derive (b : vs)
      _ -> []

main :: IO ()
main = readFile "input/day09.txt" >>= print . solve . parse
