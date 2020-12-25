#!/usr/bin/env stack
-- stack --resolver lts-10.2 script

{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.List (transpose)
import Data.List.Split (splitOn)
import qualified Data.Set as S

data Tile = Tile {tid :: Int, tborders :: [[Bool]]} deriving (Show, Eq, Ord)

data Orientation = Orientation {flipped :: Bool, rotaton :: Int} deriving (Show, Eq, Ord)

boolsToInt :: [Bool] -> Int
boolsToInt l = sum [2 ^ e | (d, e) <- zip l [0 :: Int ..], d]

orientations :: [Orientation]
orientations = [Orientation f n | f <- [False, True], n <- [0 .. 3]]

orientTile :: [[Bool]] -> Orientation -> [[Bool]]
orientTile t (Orientation f r) = foldr ($) t' (replicate r $transpose . reverse)
  where
    t'
      | f = reverse t
      | otherwise = t

parse :: String -> [Tile]
parse s = rawTiles
  where
    rawTiles = pt . lines <$> splitOn "\n\n" s
    pt l = Tile (tilenum $ head l) $ map (== '#') <$> tail l
    tilenum x = read . init $ (!! 1) . words $ x

-- >>> solve "Tile 2311:\n..##.#..#.\n##..#.....\n#...##..#.\n####.#...#\n##.##.###.\n##...#.###\n.#.#.#..##\n..#....#..\n###...#.#.\n..###..###\n\nTile 1951:\n#.##...##.\n#.####...#\n.....#..##\n#...######\n.##.#....#\n.###.#####\n###.##.##.\n.###....#.\n..#.#..#.#\n#...##.#..\n\nTile 1171:\n####...##.\n#..##.#..#\n##.#..#.#.\n.###.####.\n..###.####\n.##....##.\n.#...####.\n#.##.####.\n####..#...\n.....##...\n\nTile 1427:\n###.##.#..\n.#..#.##..\n.#.##.#..#\n#.#.#.##.#\n....#...##\n...##..##.\n...#.#####\n.#.####.#.\n..#..###.#\n..##.#..#.\n\nTile 1489:\n##.#.#....\n..##...#..\n.##..##...\n..#...#...\n#####...#.\n#..#.#.#.#\n...#.#.#..\n##.#...##.\n..##.##.##\n###.##.#..\n\nTile 2473:\n#....####.\n#..#.##...\n#.##..#...\n######.#.#\n.#...#.#.#\n.#########\n.###.#..#.\n########.#\n##...##.#.\n..###.#.#.\n\nTile 2971:\n..#.#....#\n#...###...\n#.#.###...\n##.##..#..\n.#####..##\n.#..####.#\n#..#.#..#.\n..####.###\n..#.#.###.\n...#.#.#.#\n\nTile 2729:\n...#.#.#.#\n####.#....\n..#.#.....\n....#..#.#\n.##..##.#.\n.#.####...\n####.#.#..\n##.####...\n##..#.##..\n#.##...##.\n\nTile 3079:\n#.#.#####.\n.#..######\n..#.......\n######....\n####.#..#.\n.#...#.##.\n#.#####.##\n..#.###...\n..#.......\n..#.###..."
-- (20899048083289,2)

solve :: String -> (Int, Int)
solve s = (first, second)
  where
    first = product $ fst <$> uniqueEdges
    second = 2
    tiles = parse s
    possibleEdges (Tile ti t) = (ti, S.fromList $ boolsToInt . head . orientTile t <$> orientations)
    tileEdges = possibleEdges <$> tiles
    uniqueEdges = [(t1, e) | (t1, e1) <- tileEdges, let e = S.difference e1 $ S.unions $ snd <$> filter ((/= t1) . fst) tileEdges, S.size e == 4]

main :: IO ()
main = readFile "input/day20.txt" >>= print . solve