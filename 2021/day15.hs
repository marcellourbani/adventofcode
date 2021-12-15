#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import qualified Data.Vector as V

type Grid = V.Vector (V.Vector Int)

data Input = Input {maxx :: Int, maxy :: Int, grid :: Grid} deriving (Show)

gridAt :: Grid -> Int -> Int -> Int
gridAt g x y = g V.! y V.! x

parse :: String -> Input
parse s = Input {maxx = mx, maxy = my, grid = V.fromList $ V.fromList <$> depthlist}
  where
    depthlist :: [[Int]]
    depthlist = map (read . (: [])) <$> lines s
    my = length depthlist - 1
    mx = length (head depthlist) - 1

dijkstra :: Input -> M.Map (Int, Int) Int
dijkstra (Input mx my gr) = go initial
  where
    go m = if (mx + 1) * (my + 1) > M.size m then go $step m else m
    initial = M.singleton (0, 0) 0
    step m = M.union m newnodes
      where
        visited = M.keysSet m
        tovisit = S.difference (S.unions $ neighbors <$> S.toList visited) visited
        newnodes = M.fromList $ cost <$> S.toList tovisit
        cost (a, b) = ((a, b), mincost + gridAt gr a b)
          where
            known = S.toList $ neighbors (a, b)
            mincost = minimum . catMaybes $ M.lookup <$> known <*> pure m
        neighbors (a, b) = S.fromList [(x, y) | x <- [mina .. maxa], y <- [minb .. maxb], (x, y) /= (a, b), a == x || b == y]
          where
            minb = max 0 $ b - 1
            maxb = min my $ b + 1
            mina = max 0 $ a - 1
            maxa = min mx $ a + 1

-- >>> solve  $parse "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581"
-- (40,0)

solve :: Input -> (Int, Int)
solve (Input mx my g) = (distances M.! (mx, my), 0)
  where
    distances = dijkstra (Input mx my g)

main :: IO ()
main = readFile "input/day15.txt" >>= print . solve . parse
