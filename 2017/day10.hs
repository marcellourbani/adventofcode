#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (Foldable (foldl'))
import Data.List.Split (splitOn)

data State = State {slist :: [Int], sindex :: Int, sskip :: Int} deriving (Show, Eq)

parse :: String -> [Int]
parse s = read <$> splitOn "," s

-- >>> solve 4 $ parse "3, 4, 1, 5"
-- ([0,1,2,3,4],[3,4,1,5])

move :: State -> Int -> State
move (State l i s) n = State l''' i' (s + 1)
  where
    ll = length l
    i' = mod (i + n + s) ll
    l' = drop i l ++ take i l
    l'' = reverse (take n l') ++ drop n l'
    l''' = drop (ll - i) l'' ++ take (ll - i) l''

-- >>> solve 4 $ parse "3, 4, 1, 5"
-- 12

solve :: Int -> [Int] -> Int
solve n l = a * b
  where
    (State (a : b : _) _ _) = foldl' move initial l
    initial = State [0 .. n] 0 0

main :: IO ()
main = readFile "input/day10.txt" >>= print . solve 255 . parse
