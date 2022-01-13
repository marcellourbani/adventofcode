#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

parse :: String -> [(Int, Int)]
parse i = go (read . (: "") <$> i) 0 0
  where
    dumpc cur count = [(count, cur) | count > 0]
    go s cur count = case s of
      [] -> dumpc cur count
      (x : xs) -> if x == cur then go xs cur (count + 1) else dumpc cur count <> go xs x 1

-- >>> solve $ parse "1"
-- (82350,1166642)

solve :: [(Int, Int)] -> (Int, Int)
solve l = (sum $ fst <$> generations !! 40, sum $ fst <$> generations !! 50)
  where
    nextp (n, c) = if c == n then [(2, n)] else [(1, n), (1, c)]
    comp i = case i of
      (c1, n1) : (c2, n2) : xs | n2 == n1 -> (c1 + c2, n1) : comp xs
      x1 : x2 : xs -> x1 : comp (x2 : xs)
      _ -> i
    say (c, n) = show c <> show n
    sayl i = i >>= say
    next i = comp $ i >>= nextp
    generations = iterate next l

main :: IO ()
main = readFile "input/day10.txt" >>= print . solve . parse
