#!/usr/bin/env stack
-- stack --resolver lts-12.21 script

{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.List.Split (splitOn)
import qualified Data.Set as S

-- >>> solve "Player 1:\n9\n2\n6\n3\n1\n\nPlayer 2:\n5\n8\n4\n7\n10"
-- (306,291)

solve :: String -> (Int, Int)
solve s = (first, second)
  where
    first = sum $ uncurry (*) <$> zip [1 ..] (reverse outcome)
    second = sum $ uncurry (*) <$> zip [1 ..] (reverse outcome2)
    input = fmap read . tail . lines <$> splitOn "\n\n" s
    outcome = game (head input) $ input !! 1
    outcome2 = snd $ recgame (head input) (input !! 1) S.empty S.empty
    recgame :: [Int] -> [Int] -> S.Set [Int] -> S.Set [Int] -> (Int, [Int])
    recgame x y px py
      | S.member x px && S.member y py = (1, x)
      | otherwise = case (x, y) of
        (_, []) -> (1, x)
        ([], _) -> (2, y)
        (x1 : xs, y1 : ys)
          | winner == 1 -> recgame (xs ++ [x1, y1]) ys (S.insert x px) (S.insert y py)
          | winner == 2 -> recgame xs (ys ++ [y1, x1]) (S.insert x px) (S.insert y py)
          | otherwise -> (0, [])
          where
            okx = length xs >= x1
            oky = length ys >= y1
            winner
              | okx && oky = fst $ recgame (take x1 xs) (take y1 ys) S.empty S.empty
              | x > y = 1
              | otherwise = 2

    game xs ys = case (xs, ys) of
      ([], _) -> ys
      (_, []) -> xs
      (a : as, b : bs)
        | a > b -> game (as ++ [a, b]) bs
        | otherwise -> game as $ bs ++ [b, a]

main :: IO ()
main = readFile "input/day22.txt" >>= print . solve