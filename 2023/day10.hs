#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import Data.Foldable (find)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S

data MapTile = S | V | H | NE | NW | SE | SW deriving (Show, Eq)

type GameMap = M.Map (Int, Int) MapTile

parseDir :: Char -> Maybe MapTile
parseDir d = case d of
  '|' -> Just V
  '-' -> Just H
  'L' -> Just NE
  'J' -> Just NW
  '7' -> Just SW
  'F' -> Just SE
  'S' -> Just S
  _ -> Nothing

parse :: String -> GameMap
parse s = M.fromList $ zip [0 ..] (lines s) >>= parseLine
  where
    parseLine (y, l) = valids $ zip ((,y) <$> [0 ..]) $ parseDir <$> l
    valids l = case l of
      [] -> []
      (_, Nothing) : xs -> valids xs
      (a, Just t) : xs -> (a, t) : valids xs

validLocation :: (Int, Int) -> (Int, Int) -> MapTile -> Bool
validLocation (sx, sy) (ex, ey) d = case (d, ex - sx, ey - sy) of
  (H, 1, 0) -> True
  (H, -1, 0) -> True
  (V, 0, 1) -> True
  (V, 0, -1) -> True
  (NE, 0, 1) -> True
  (NW, 0, 1) -> True
  (SW, 0, -1) -> True
  (SE, 0, -1) -> True
  (SW, 1, 0) -> True
  (NW, 1, 0) -> True
  (NE, -1, 0) -> True
  (SE, -1, 0) -> True
  (S, _, _) -> True
  _ -> False

adjacents :: GameMap -> S.Set (Int, Int) -> (Int, Int) -> [((Int, Int), MapTile)]
adjacents m blacklist p@(x, y) = mapMaybe vmap ls
  where
    vmap k = (k,) <$> M.lookup k m
    valid p1 = p1 /= p && S.notMember p1 blacklist
    ls = filter valid $ (,) <$> [x - 1 .. x + 1] <*> [y - 1 .. y + 1]

validAdjacents :: GameMap -> S.Set (Int, Int) -> (Int, Int) -> [((Int, Int), MapTile)]
validAdjacents m blacklist p@(x, y) = filter valid base
  where
    base = adjacents m blacklist p
    cur = m M.! p
    valid (p1, t) = validLocation p p1 t && validLocation p1 p cur

distances :: GameMap -> (Int, Int) -> M.Map (Int, Int) Int
distances gamemap startpos = go initial [startpos] 1
  where
    initial = M.singleton startpos 0
    go curr ps dist
      | null newkeys = curr
      | otherwise = go nxt newkeys (dist + 1)
      where
        bl = M.keysSet curr
        newkeys = fst <$> (ps >>= validAdjacents gamemap bl)
        nxt = M.union curr $ M.fromList ((,dist) <$> newkeys)

fill :: S.Set (Int, Int) -> S.Set (Int, Int) -> S.Set (Int, Int)
fill cur cand
  | S.null cand = cur
  | S.null toadd = cur
  | otherwise = fill cur' cand'
  where
    neigh (x, y) = (,) <$> [x - 1 .. x + 1] <*> [y - 1 .. y + 1]
    toadd = S.intersection cand $ S.fromList $ S.toList cur >>= neigh
    cur' = S.union cur toadd
    cand' = S.difference cand toadd

enclosed :: S.Set (Int, Int) -> S.Set (Int, Int)
enclosed path = go S.empty candidates
  where
    limits se = (head xs, last xs, head ys, last ys)
      where
        xs = S.toAscList $ S.map fst se
        ys = S.toAscList $ S.map snd se
    (x1, x2, y1, y2) = limits path
    candidates = S.difference (S.fromList $ (,) <$> [x1 .. x2] <*> [y1 .. y2]) path
    isOut s = external
      where
        (xx1, xx2, yy1, yy2) = limits s
        external = xx1 == x1 || xx2 == x2 || yy1 == y1 || yy2 == y2
    go inside cand
      | S.null cand = inside
      | otherwise = go inside' cand'
      where
        block = fill (S.take 1 cand) cand
        inside' = if isOut block then inside else S.union block inside
        cand' = S.difference cand block

-- >>> solve $ parse "-L|F7\n7S-7|\nL|7||\n-L-J|\nL|-JF"
-- >>> solve $ parse "..F7.\n.FJ|.\nSJ.L7\n|F--J\nLJ..."
-- >>> solve $ parse ".F----7F7F7F7F-7....\n.|F--7||||||||FJ....\n.||.FJ||||||||L7....\nFJL7L7LJLJ||LJ.L-7..\nL--J.L7...LJS7F-7L7.\n....F-J..F7FJ|L7L7L7\n....L7.F7||L7|.L7L7|\n.....|FJLJ|FJ|F7|.LJ\n....FJL-7.||.||||...\n....L---J.LJ.LJLJ..."
-- >>> solve $ parse "FF7FSF7F7F7F7F7F---7\nL|LJ||||||||||||F--J\nFL-7LJLJ||||||LJL-77\nF--JF--7||LJLJIF7FJ-\nL---JF-JLJIIIIFJLJJ7\n|F|F-JF---7IIIL7L|7|\n|FFJF7L7F-JF7IIL---7\n7-L-JL7||F7|L7F-7F7|\nL.L7LFJ|||||FJL7||LJ\nL7JLJL-JLJLJL--JLJ.L"
-- (4,1)
-- (8,1)
-- (70,9)
-- (80,10)

-- solve :: GameMap -> Int
solve gamemap = (p1, p2)
  where
    path = distances gamemap startP
    p1 = maximum path
    p2 = S.size $ enclosed $ M.keysSet path
    startP = fst . M.elemAt 0 $ M.filter (== S) gamemap

main :: IO ()
main = readFile "input/day10.txt" >>= print . solve . parse
