#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Control.Lens (re, (^.))
import Data.Bits (xor)
import Data.Char (chr)
import Data.Foldable (Foldable (foldl'))
import Data.List.Split (splitOn)
import Numeric.Lens (hex)

data State = State {slist :: [Int], sindex :: Int, sskip :: Int} deriving (Show, Eq)

parse :: String -> ([Int], [Int])
parse s = (read <$> splitOn "," s, fromEnum <$> s)

move :: State -> Int -> State
move (State l i s) n = State l''' i' (s + 1)
  where
    ll = length l
    i' = mod (i + n + s) ll
    l' = drop i l ++ take i l
    l'' = reverse (take n l') ++ drop n l'
    l''' = drop (ll - i) l'' ++ take (ll - i) l''

-- >>> solve 4 $ parse "3,4,1,5"
-- >>> solve 255 $ parse "1,2,3"
-- (12,"04")
-- (0,"3efbe78a8d82f29979031a4aa0b16a9d")

solve :: Int -> ([Int], [Int]) -> (Int, String)
solve n (l, l2) = (a * b, p2)
  where
    (State (a : b : _) _ _) = foldl' move initial l
    initial = State [0 .. n] 0 0
    h v
      | v >= 16 = v ^. re hex
      | otherwise = '0' : v ^. re hex
    p2 = reduce sparse >>= h
      where
        sparse = slist $ foldl' move initial l2'
        base = l2 <> [17, 31, 73, 47, 23]
        l2' = take (64 * length base) $ cycle base
        reduce l = case take 16 l of
          [] -> []
          l' -> foldl' xor 0 l' : reduce (drop 16 l)

main :: IO ()
main = readFile "input/day10.txt" >>= print . solve 255 . parse
