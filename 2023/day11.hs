#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import qualified Data.Set as S

type GameMap = S.Set (Int, Int)

parse :: String -> GameMap
parse s = S.fromList [(x, y) | (y, l) <- zip [0 ..] (lines s), (x, c) <- zip [0 ..] l, c == '#']

expand :: GameMap -> GameMap
expand m = S.map expp m
  where
    maxy = maximum $ snd <$> S.toList m
    maxx = maximum $ fst <$> S.toList m
    expp (x, y) = (x + length (filter (< x) emptycolumns), y + length (filter (< y) emptylines))
    emptycolumns = [x | x <- [0 .. maxx], S.null $ S.intersection m $ S.fromList $ (x,) <$> [0 .. maxy]]
    emptylines = [y | y <- [0 .. maxy], S.null $ S.intersection m $ S.fromList $ (,y) <$> [0 .. maxx]]

allDist :: GameMap -> Int
allDist m = sum $ manhdist <$> pairs
  where
    pairs = [(a, b) | a <- S.toList m, b <- S.toList m, a > b]
    manhdist ((x1, y1), (x2, y2)) = abs (x1 - x2) + abs (y1 - y2)

expandedDist :: GameMap -> Int -> Int
expandedDist m exf = sum $ expdist <$> pairs
  where
    maxy = maximum $ snd <$> S.toList m
    maxx = maximum $ fst <$> S.toList m
    within a b x = (x > a && x < b) || (x > b && x < a)
    toexpand (x, y) (x2, y2) = length $ filter (within x x2) emptycolumns <> filter (within y y2) emptylines
    emptycolumns = [x | x <- [0 .. maxx], S.null $ S.intersection m $ S.fromList $ (x,) <$> [0 .. maxy]]
    emptylines = [y | y <- [0 .. maxy], S.null $ S.intersection m $ S.fromList $ (,y) <$> [0 .. maxx]]
    pairs = [(a, b) | a <- S.toList m, b <- S.toList m, a > b]
    expdist ((x1, y1), (x2, y2)) = abs (x1 - x2) + abs (y1 - y2) + ((exf - 1) * toexpand (x1, y1) (x2, y2))

-- >>> solve 100 $ parse "...#......\n.......#..\n#.........\n..........\n......#...\n.#........\n.........#\n..........\n.......#..\n#...#....."
-- (374,8410)

solve :: Int -> GameMap -> (Int, Int)
solve exf gamemap = (allDist $ expand gamemap, expandedDist gamemap exf)

main :: IO ()
main = readFile "input/day11.txt" >>= print . solve 1000000 . parse
