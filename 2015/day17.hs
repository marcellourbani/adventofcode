#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

parse :: String -> [Int]
parse i = read <$> lines i

sumTo :: Int -> [Int] -> [[Int]]
sumTo t l = case (l, t) of
  (_, 0) -> [[]]
  ([], _) -> []
  (_, v) | v < 0 -> []
  (x : xs, _) -> ((x :) <$> sumTo (t - x) xs) <> sumTo t xs

sumTol :: Int -> Int -> [Int] -> [[Int]]
sumTol t n l = case (l, t, n) of
  (_, 0, _) -> [[]]
  (_, _, 0) -> []
  ([], _, _) -> []
  (_, v, _) | v < 0 -> []
  (x : xs, _, _) -> ((x :) <$> sumTol (t - x) (n -1) xs) <> sumTol t n xs

-- >>> sumTol 25 2 $ parse "20\n15\n10\n5\n5"
-- [[20,5],[20,5],[15,10]]

solve :: [Int] -> (Int, Int)
solve l = (length p1s, length p2s)
  where
    p1s = sumTo 150 l
    minLen = minimum $ length <$> p1s
    p2s = sumTol 150 minLen l

main :: IO ()
main = readFile "input/day17.txt" >>= print . solve . parse
