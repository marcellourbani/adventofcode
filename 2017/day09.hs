#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

-- >>> solve "{{<a!>},{<a!>},{<a!>},{<ab>}}"
-- (3,17)
solve :: String -> (Int, Int)
solve l = go l False False 0 1 0
  where
    go l garbage escaped acc d g = case (l, garbage, escaped) of
      ([], _, _) -> (acc, g)
      (x : xs, False, False) -> case x of
        '{' -> go xs False False (acc + d) (d + 1) g
        '<' -> go xs True False acc d g
        '}' -> go xs False False acc (d - 1) g
        _ -> go xs False False acc d g
      (x : xs, True, False) -> case x of
        '>' -> go xs False False acc d g
        '!' -> go xs True True acc d g
        _ -> go xs True False acc d (g + 1)
      (x : xs, True, True) -> go xs True False acc d g
      (x : xs, gg, e) -> go xs gg e acc d g

main :: IO ()
main = readFile "input/day09.txt" >>= print . solve
