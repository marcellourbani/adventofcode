#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Char (toUpper)
import Data.Foldable (Foldable (foldl'))

data Directions = N | NE | NW | S | SE | SW deriving (Show, Eq, Read)

parse :: String -> [Directions]
parse s = read $ "[" ++ (toUpper <$> s) ++ "]"

move :: (Int, Int) -> Directions -> (Int, Int)
move (x, y) d = case d of
  N -> (x, y + 2)
  NE -> (x + 1, y + 1)
  NW -> (x - 1, y + 1)
  S -> (x, y - 2)
  SE -> (x + 1, y - 1)
  SW -> (x - 1, y - 1)

-- >>> solve $ parse "se,sw,se,sw,sw"
-- (3,3)
solve :: [Directions] -> (Int, Int)
solve l = (p1, go (0, 0) l 0)
  where
    p1 = dist $ foldl' move (0, 0) l
    dist (x, y) = if xa > ya then xa else xa + div (ya - xa) 2
      where
        (xa, ya) = (abs x, abs y)
    go p l acc = case l of
      [] -> acc
      (x : xs) -> go p' xs $max (dist p') acc where p' = move p x

main :: IO ()
main = readFile "input/day11.txt" >>= print . solve . parse
