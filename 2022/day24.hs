#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List (elemIndex)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

data Direction = U | D | L | R deriving (Show)

data Valley = Valley {vMaxx :: Int, vMaxy :: Int, vStart :: Int, vEnd :: Int, vBlizs :: M.Map (Int, Int) [Direction]} deriving (Show)

parse :: String -> Valley
parse s = Valley maxx maxy enter exit $ M.fromList cs
  where
    fc c = case c of
      '^' -> U
      'v' -> D
      '<' -> L
      '>' -> R
      _ -> undefined
    enter = fromMaybe 0 $ elemIndex '.' s
    exit = fromMaybe 0 $ elemIndex '.' $ last $ lines s
    maxx = length (head $lines s) -2
    maxy = length (lines s) -2
    cs = [((x, y), [fc c]) | (y, l) <- zip [1 .. maxy] $ tail $lines s, (x, c) <- zip [1 .. maxx] $ tail l, c /= '.']

moveBlizzards :: Valley -> Valley
moveBlizzards v@(Valley mx my _ _ bs) = v {vBlizs = bs'}
  where
    mm l v = 1 + mod (v -1) l
    moveB (x, y) d = case d of
      U -> (x, mm my $ y -1)
      D -> (x, mm my $ y + 1)
      L -> (mm mx $ x -1, y)
      R -> (mm mx $ x + 1, y)
    moveBs (k, d) = case d of
      [] -> M.empty
      d1 : ds -> M.insert (moveB k d1) [d1] $ moveBs (k, ds)
    bs' = M.unionsWith (<>) $ moveBs <$> M.toList bs

-- >>> solve $ parse "#.######\n#>>.<^<#\n#.<..<<#\n#>v.><>#\n#<^v^^>#\n######.#"

solve l = take 10 $ iterate moveBlizzards l

main :: IO ()
main = readFile "input/day24.txt" >>= print . solve . parse
