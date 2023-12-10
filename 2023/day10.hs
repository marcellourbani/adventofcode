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

-- >>> solve $ parse "-L|F7\n7S-7|\nL|7||\n-L-J|\nL|-JF"
-- >>> solve $ parse "..F7.\n.FJ|.\nSJ.L7\n|F--J\nLJ..."
-- 4
-- 8

solve :: GameMap -> Int
solve gamemap = p1
  where
    p1 = maximum $ distances gamemap startP
    startP = fst . M.elemAt 0 $ M.filter (== S) gamemap

main :: IO ()
main = readFile "input/day10.txt" >>= print . solve . parse
