#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List (sort)

type Triangle = (Int, Int, Int)

parse :: String -> [Triangle]
parse s = pl . fmap read . words <$> lines s
  where
    pl [a, b, c] = (a, b, c)
    pl _ = error "parse error"

isValid :: Triangle -> Bool
isValid (a, b, c) = a + b > c && a + c > b && b + c > a

aggregate :: [Triangle] -> [Triangle]
aggregate t = case t of
  (a1, a2, a3) : (b1, b2, b3) : (c1, c2, c3) : ts -> [(a1, b1, c1), (a2, b2, c2), (a3, b3, c3)] <> aggregate ts
  _ -> []

-- >>> solve $ parse "5 10 25"
-- 0

solve :: [Triangle] -> (Int, Int)
solve l = (p1, p2)
  where
    p1 = length $ filter isValid l
    p2 = length $ filter isValid $ aggregate l

main :: IO ()
main = readFile "input/day03.txt" >>= print . solve . parse
