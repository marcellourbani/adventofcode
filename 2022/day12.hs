#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Algorithm.Search (dijkstra)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

parse :: String -> M.Map (Int, Int) Char
parse s = M.fromList [((x, y), c) | (y, l) <- zip [0 ..] $lines s, (x, c) <- zip [0 ..] l]

shortestPath :: M.Map (Int, Int) Char -> (Int, Int) -> (Int, Int) -> Int
shortestPath hmap startpos endpos = fromMaybe (maxx * maxy) solution
  where
    (maxx, maxy) = maximum $ M.keys hmap
    neighbors (x, y) = [(a, y) | a <- [x - 1, x + 1], a >= 0, a <= maxx] <> [(x, a) | a <- [y - 1, y + 1], a >= 0, a <= maxy]
    validneighbors x = [y | y <- neighbors x, valid y]
      where
        maxh = succ $ hmap M.! x
        valid p = hmap M.! p <= maxh
    cost = const . const 1
    goal = (== endpos)
    solution = fst <$> dijkstra validneighbors cost goal startpos

-- >>> solve $ parse "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi"
-- (31,29)

solve :: M.Map (Int, Int) Char -> (Int, Int)
solve m = (p1, p2)
  where
    p1 = shortestPath hmap startpos endpos
    p2starts = M.keys $ M.filter (== 'a') hmap
    p2 = minimum $ shortestPath hmap <$> p2starts <*> [endpos]
    startpos = fst . head $ M.toList $ M.filter (== 'S') m
    endpos = fst . head $ M.toList $ M.filter (== 'E') m
    hmap = M.union (M.fromList [(startpos, 'a'), (endpos, 'z')]) m

main :: IO ()
main = readFile "input/day12.txt" >>= print . solve . parse
