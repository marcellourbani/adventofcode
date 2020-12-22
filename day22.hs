#!/usr/bin/env stack
-- stack --resolver lts-12.21 script

{-# OPTIONS_GHC -Wall #-}

module Main where

-- >>> solve "Player 1:\n9\n2\n6\n3\n1\n\nPlayer 2:\n5\n8\n4\n7\n10"
-- (306,2)

import Data.List.Split (splitOn)

solve :: String -> (Int, Int)
solve s = (first, second)
  where
    first = sum $ uncurry (*) <$> zip [1 ..] (reverse outcome)
    second = 2
    input = fmap read . tail . lines <$> splitOn "\n\n" s
    outcome = game (head input) $ input !! 1
    game xs ys = case (xs, ys) of
      ([], _) -> ys
      (_, []) -> xs
      (a : as, b : bs)
        | a > b -> game (as ++ [a, b]) bs
        | otherwise -> game as $ bs ++ [b, a]

main :: IO ()
main = readFile "input/day22.txt" >>= print . solve