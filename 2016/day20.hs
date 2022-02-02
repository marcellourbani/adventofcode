#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List (sortOn)
import Data.List.Split (splitOn)
import Data.Tuple (swap)

parse :: String -> [(Int, Int)]
parse s = pl <$> lines s
  where
    pl l = (read a, read b) where [a, b] = splitOn "-" l

-- >>> solve $ parse "5-8\n0-2\n4-7"
-- 3
solve::[(Int,Int)]->(Int,Int)
solve l = (go 0 sl [] False,2^32 - banned)
  where
    sl = sortOn fst l
    br (b,e) = e-b+1
    banned = sum $ br <$> jr sl 
    jr cs = case cs of
      a@(b1,e1) : b@(b2,e2) : xs | b2 <= (e1+1) -> jr $(b1,max e1 e2):xs
                                 | otherwise -> a:jr (b:xs)
      xs -> xs
    go cur l ot swapped = case l of
      []
        | swapped -> go cur (reverse ot) [] False
        | otherwise -> cur
      (a, b) : xs
        | a <= cur && b >= cur -> go (b + 1) xs ot True
        | otherwise -> go cur xs ((a, b) : ot) swapped

main :: IO ()
main = readFile "input/day20.txt" >>= print . solve . parse


