#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (find)
import Data.List (tails)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import GHC.Float (int2Double)

parse :: String -> Int
parse = read

spiralCoord :: Int -> (Int, Int)
spiralCoord n
  | n == 1 = (0, 0)
  | otherwise = case side of
    0 -> (half, ofs + 1 - half)
    1 -> (half - ofs -1, half)
    2 -> (- half, half - ofs -1)
    _ -> (- half + ofs + 1, - half)
  where
    (side, ofs, half) = go 1
    go b
      | b ^ 2 < n = go $ b + 2
      | otherwise = (l, lo, c)
      where
        r = n - (b -2) ^ 2
        (l, lo) = divMod (r -1) (b -1)
        c = div b 2

-- >>> solve $parse "1024"
-- (31,1968)

solve :: Int -> (Int, Int)
solve l = (s, p2 initial 2)
  where
    initial = M.singleton (0, 0) 1
    p2 m c
      | nv > l = nv
      | otherwise = p2 (M.insert (x, y) nv m) (c + 1)
      where
        (x, y) = spiralCoord c
        nv = sum $ catMaybes [M.lookup (a, b) m | a <- [x -1, x, x + 1], b <- [y -1, y, y + 1]]
    s = go 1
    go n
      | l == 1 = 0
      | n ^ 2 < l = go (n + 2)
      | otherwise = c + toc
      where
        r = l - (n -2) ^ 2
        c = div n 2
        toc = abs (c - r `mod` (n -1))

main :: IO ()
main = readFile "input/day03.txt" >>= print . solve . parse
