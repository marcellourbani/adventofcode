#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

parse :: String -> Int
parse = read

spinLock :: Int -> Int -> Int
spinLock step n = go [0] 0 1
  where
    go l p c
      | c == (n + 1) = l !! ((p + 1) `mod` cursize)
      | otherwise = go l' p' c'
      where
        cursize = length l
        p' = 1 + (p + step) `mod` cursize
        c' = c + 1
        l' = take p' l <> [c] <> drop p' l

spinLock2 :: Int -> Int -> Int
spinLock2 step n = go 0 1 0
  where
    go p c l
      | c == (n + 1) = l
      | p' == 1 = go p' c' c
      | otherwise = go p' c' l
      where
        c' = c + 1
        p' = 1 + (p + step) `mod` c

-- >>> solve $ parse "3"
-- (638,1222153)
solve :: Int -> (Int, Int)
solve l = (p1, p2)
  where
    p1 = spinLock l 2017
    p2 = spinLock2 l 50000000

main :: IO ()
main = readFile "input/day17.txt" >>= print . solve . parse
