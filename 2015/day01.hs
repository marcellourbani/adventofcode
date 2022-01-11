#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

-- >>> solve "())("
-- (0,3)
solve :: String -> (Int, Int)
solve l = (sum deltas, firstb deltas 0 0)
  where
    cpar c = if c == '(' then 1 else -1
    deltas = cpar <$> l
    firstb ll acc p = case ll of
      [] -> p
      _ | acc == -1 -> p
      (x : xs) -> firstb xs (acc + x) (p + 1)

main :: IO ()
main = readFile "input/day01.txt" >>= print . solve
