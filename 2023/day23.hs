#!/usr/bin/env stack
-- stack --resolver lts-18.18 script --optimize

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

data GameMap c = GameMap {gmW :: Int, gmH :: Int, gmMap :: M.Map (Int, Int) c} deriving (Eq)

instance Functor GameMap where
  fmap f (GameMap w h m) = GameMap w h $ M.map f m

instance Show (GameMap Char) where
  show gm@(GameMap w h m) = unlines [[mapTile '.' gm (x, y) | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]

mapTile :: c -> GameMap c -> (Int, Int) -> c
mapTile def gm p = fromMaybe def $ M.lookup p $ gmMap gm

inMap :: GameMap c -> (Int, Int) -> Bool
inMap (GameMap w h _) (x, y) = 0 <= x && x < w && 0 <= y && y < h

parse :: String -> GameMap Char
parse s = GameMap w h $ M.fromList [((x, y), c) | (y, line) <- zip [0 ..] l, (x, c) <- zip [0 ..] line, c /= '.']
  where
    l = lines s
    w = length $ head l
    h = length l

nexts :: (Int, Int) -> Char -> [(Int, Int)]
nexts (x, y) c = case c of
  '>' -> [(x + 1, y)]
  '<' -> [(x - 1, y)]
  '^' -> [(x, y - 1)]
  'v' -> [(x, y + 1)]
  _ -> ((,y) <$> [x - 1, x + 1]) <> ((x,) <$> [y - 1, y + 1])

nexts2 :: (Int, Int) -> Char -> [(Int, Int)]
nexts2 (x, y) _ = ((,y) <$> [x - 1, x + 1]) <> ((x,) <$> [y - 1, y + 1])

longestWalk :: GameMap Char -> ((Int, Int) -> Char -> [(Int, Int)]) -> M.Map (Int, Int) Int
longestWalk gm nextf = dfs M.empty M.empty [(1, 0)]
  where
    mt = mapTile '.' gm
    inmap = inMap gm
    exit = (gmW gm - 2, gmH gm - 1)
    dfs visited best l = case l of
      [] | M.notMember exit visited -> best
      [] | length visited > length best -> visited
      [] -> best
      p : _ | p == exit -> dfs (M.insert p 1 visited) best []
      p : ps -> dfs visited best' ps
        where
          curlen = 1 + length visited
          visited' = M.insert p curlen visited
          seen x = M.notMember x visited
          valid x = seen x && inmap x && mt x /= '#' && improving x
          improving x = curlen + 1000 > M.findWithDefault 0 x best
          l' = filter valid $ nextf p (mt p)
          best' = dfs visited' best l'

-- >>> solve $ parse "#.#####################\n#.......#########...###\n#######.#########.#.###\n###.....#.>.>.###.#.###\n###v#####.#v#.###.#.###\n###.>...#.#.#.....#...#\n###v###.#.#.#########.#\n###...#.#.#.......#...#\n#####.#.#.#######.#.###\n#.....#.#.#.......#...#\n#.#####.#.#.#########v#\n#.#...#...#...###...>.#\n#.#.#v#######v###.###v#\n#...#.>.#...>.>.#.###.#\n#####v#.#.###v#.#.###.#\n#.....#...#...#.#.#...#\n#.#########.###.#.#.###\n#...###...#...#...#.###\n###.###.#.###v#####v###\n#...#...#.#.>.>.#.>.###\n#.###.###.#.###.#.#v###\n#.....###...###...#...#\n#####################.#"
-- (94,154)

solve :: GameMap Char -> (Int, Int)
solve gm = (p1, p2)
  where
    p1 = length (longestWalk gm nexts) - 1
    p2 = length (longestWalk gm nexts2) - 1

main :: IO ()
main = readFile "input/day23.txt" >>= print . solve . parse
