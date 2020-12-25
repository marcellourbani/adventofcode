#!/usr/bin/env stack
-- stack --resolver lts-10.2 script

{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.Foldable (find)
import Data.List (foldl1', transpose)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S

data Tile = Tile {tid :: Int, ttab :: [[Bool]]} deriving (Show, Eq, Ord)

data Orientation = Orientation {flipped :: Bool, rotaton :: Int} deriving (Show, Eq, Ord)

boolsToInt :: [Bool] -> Int
boolsToInt l = sum [2 ^ e | (d, e) <- zip l [0 :: Int ..], d]

orientations :: [Orientation]
orientations = [Orientation f n | f <- [False, True], n <- [0 .. 3]]

orientTile :: Tile -> Orientation -> Tile
orientTile (Tile ti t) (Orientation f r) = Tile ti $ foldr ($) t' (replicate (mod r 4) $transpose . reverse)
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

topNum :: Tile -> Int
topNum = boolsToInt . head . ttab

-- >>> solve "Tile 2311:\n..##.#..#.\n##..#.....\n#...##..#.\n####.#...#\n##.##.###.\n##...#.###\n.#.#.#..##\n..#....#..\n###...#.#.\n..###..###\n\nTile 1951:\n#.##...##.\n#.####...#\n.....#..##\n#...######\n.##.#....#\n.###.#####\n###.##.##.\n.###....#.\n..#.#..#.#\n#...##.#..\n\nTile 1171:\n####...##.\n#..##.#..#\n##.#..#.#.\n.###.####.\n..###.####\n.##....##.\n.#...####.\n#.##.####.\n####..#...\n.....##...\n\nTile 1427:\n###.##.#..\n.#..#.##..\n.#.##.#..#\n#.#.#.##.#\n....#...##\n...##..##.\n...#.#####\n.#.####.#.\n..#..###.#\n..##.#..#.\n\nTile 1489:\n##.#.#....\n..##...#..\n.##..##...\n..#...#...\n#####...#.\n#..#.#.#.#\n...#.#.#..\n##.#...##.\n..##.##.##\n###.##.#..\n\nTile 2473:\n#....####.\n#..#.##...\n#.##..#...\n######.#.#\n.#...#.#.#\n.#########\n.###.#..#.\n########.#\n##...##.#.\n..###.#.#.\n\nTile 2971:\n..#.#....#\n#...###...\n#.#.###...\n##.##..#..\n.#####..##\n.#..####.#\n#..#.#..#.\n..####.###\n..#.#.###.\n...#.#.#.#\n\nTile 2729:\n...#.#.#.#\n####.#....\n..#.#.....\n....#..#.#\n.##..##.#.\n.#.####...\n####.#.#..\n##.####...\n##..#.##..\n#.##...##.\n\nTile 3079:\n#.#.#####.\n.#..######\n..#.......\n######....\n####.#..#.\n.#...#.##.\n#.#####.##\n..#.###...\n..#.......\n..#.###..."
-- (20899048083289,[2,2,1,1,1,1,2,2,1,2,1,1,2,2,2,1,2,1,1,2,2,2,2,1,2,2,1,1,2,1,1,1,1,2,2,1,2,2,2,1,2,1,1,1,1,2,1,2])

-- solve :: String -> (Int, Int)
solve s = (first, second)
  where
    first = product $ fst <$> corners
    second = length . snd <$> M.toList edgeMap
    tiles = parse s
    possibleEdges (Tile ti t) = (ti, S.fromList $ topNum . orientTile (Tile ti t) <$> orientations)
    tileEdges = possibleEdges <$> tiles
    uniqueEdges = [(t1, e) | (t1, e1) <- tileEdges, let e = S.difference e1 $ S.unions $ snd <$> filter ((/= t1) . fst) tileEdges]
    edgeMap = M.fromListWith (++) [(e, [t]) | (t, es) <- tileEdges, e <- S.toList es]
    tileMap = M.fromList [(tid t, t) | t <- tiles]
    corners = filter ((== 4) . S.size . snd) uniqueEdges
    rotate tr r = orientTile tr $ Orientation False r
    firstCorner = topRight
      where
        (ti, ue) = head corners
        tt = tileMap M.! ti
        ors = [i | i <- [0 .. 3], let a = topNum $ rotate tt i, S.member a ue]
        topRight = orientTile tt $ Orientation False $ head [i | i <- ors, mod (i - 1) 4 `elem` ors]
    toright t = other
      where
        shared = topNum $ rotate t 4
        toLeft (Orientation f r) = Orientation f $ mod (r -1) 4
        transform ot = head [orientTile ot (toLeft o) | o <- orientations, topNum (orientTile ot o) == shared]
        other = fmap transform . flip M.lookup tileMap =<< find (/= tid t) (edgeMap M.! shared)
    line t = case toright t of
      Nothing -> [t]
      Just t1 -> t : line t1
    tobelow t = other
      where
        shared = topNum $ rotate t 2
        transform ot = head [orientTile ot o | o <- orientations, topNum (orientTile ot o) == shared]
        other = fmap transform . flip M.lookup tileMap =<< find (/= tid t) (edgeMap M.! shared)
    column t = case tobelow t of
      Nothing -> [t]
      Just t1 -> t : column t1
    picture = Tile 0 $ foldl1' (zipWith (++)) $foldl1' (zipWith (++)) <$> (map ttab <$> grid)
      where
        mid = tail . init
        clearTile (Tile i t) = Tile i (mid $ mid <$> t)
        grid = map clearTile <$> map line (column firstCorner)

main :: IO ()
main = readFile "input/day20.txt" >>= print . solve