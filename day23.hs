#!/usr/bin/env stack
-- stack --resolver lts-12.21 script

{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

-- >>> solve "389125467"
-- ProgressCancelledException

import Data.List.Split (splitOn)

solve :: String -> (String, Int)
solve s = (first, second)
  where
    first = concat $ show <$> game input 100
    second = product $ take 2 $ game (input ++ [10 .. 1000000]) 10000
    input :: [Int]
    input = read . (: []) <$> s
    maxi = maximum input
    game !i sn
      | sn == 0 = pos ++ pre
      | otherwise = game (step i) $ sn -1
      where
        [pre, pos] = splitOn [1] i
    step [] = []
    step (x : xs) = pre ++ (next : cur ++ pos ++ [x])
      where
        cur = take 3 xs
        next = head [nx | i <- [1 .. maxi], let nx = 1 + mod (x - i -1) maxi, nx `notElem` cur]
        [pre, pos] = splitOn [next] $ drop 3 xs

main :: IO ()
main = print $ solve "362981754"