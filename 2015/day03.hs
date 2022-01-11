#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (Foldable (foldl'))
import qualified Data.Set as S

-- >>> solve "^v^v^v^v^v"
-- (2,11)

solve :: String -> (Int, Int)
solve l = (length $ go2 l, length $ S.union (go2 sl) (go2 rl))
  where
    movec (x, y) c = case c of
      '^' -> (x, y -1)
      'v' -> (x, y + 1)
      '<' -> (x -1, y)
      '>' -> (x + 1, y)
      _ -> (x, y)
    go2 i = go i (0, 0) $ S.singleton (0, 0)
    (sl, rl) = foldr (\x (xs, ys) -> (x : ys, xs)) ([], []) l
    go i cur acc = case i of
      [] -> acc
      (c : cs) -> go cs nv (S.insert nv acc) where nv = movec cur c

main :: IO ()
main = readFile "input/day03.txt" >>= print . solve
