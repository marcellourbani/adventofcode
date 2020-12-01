#!/usr/bin/env stack
-- stack --resolver lts-10.2 script
module Main where

-- Finds the couples that add up to 2020
-- >>> couples [1721,979,366,299,675,1456]
-- [(1721,299)]
couples :: [Int] -> [(Int, Int)]
couples l = [(x,y)| k <- zip l [1..],let x = fst k ,y <- drop (snd k) l,x + y == 2020]

-- Finds the triplets that add up to 2020
-- >>> triplets [1721,979,366,299,675,1456]
-- [(979,366,675)]
triplets :: [Int] -> [(Int, Int, Int)]
triplets l = [(x,y,z)| k <- zip l [1..],let x = fst k ,y <- drop (snd k) l,z <- drop (snd k + 1) l,x + y + z == 2020]

-- finds the couples and triplets that add to 2020, and multiplies them
-- >>> solve [1721,979,366,299,675,1456]
-- (514579,241861950)
solve:: [Int] -> ( Int, Int)
solve l = (a*b ,c*d*e) where
    (a,b) = head $ couples l
    (c,d,e) = head $ triplets l

main :: IO ()
main = interact $ show.solve.fmap read . words
