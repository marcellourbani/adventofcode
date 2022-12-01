#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as M

parse :: String -> [(Int, Int)]
parse s = pl <$> lines (filter (/= ':') s)
  where
    pl l = let [a, b] = read <$> words l in (a, b)

-- >>> solve $ parse "0: 3\n1: 2\n4: 4\n6: 4"
-- (24,10)
solve :: [(Int,Int)] -> (Int, Int)
solve l = (p1 l, p2 l)
  where
    p2 a = head [x | x<-[0..],and $(==0). sev x <$> a]
    p1 l = sum $ sev 0 <$> l
    sev wait (a, b)
      | mod (wait +a) (2 * b -2) == 0 = (wait+a) * b
      | otherwise = 0

main :: IO ()
main = readFile "input/day13.txt" >>= print . solve . parse
