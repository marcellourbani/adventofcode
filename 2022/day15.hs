#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import qualified Data.Set as S

data Reading = Reading {rSensor :: (Int, Int), rBeacon :: (Int, Int)} deriving (Show)

parse :: String -> [Reading]
parse s = parseline <$> lines s
  where
    parseline l = Reading (sx, sy) (bx, by) where [[sx, sy], [bx, by]] = parsePoint <$> splitOn ": closest beacon is at " (drop 10 l)
    parsePoint p = read . drop 2 <$> splitOn ", " p

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

segment :: Int -> Int -> Reading -> Maybe (Int, Int)
segment maxxy y (Reading se@(sx, sy) be)
  | maxd >= 0 = Just (max (sx - maxd) 0, min (sx + maxd) maxxy)
  | otherwise = Nothing
  where
    maxd = manhattan se be - abs (sy - y)

joinSegs :: [Maybe (Int, Int)] -> [(Int, Int)]
joinSegs s = go $ sort $ catMaybes s
  where
    go [] = []
    go [x] = [x]
    go (a@(mina, maxa) : b@(minb, maxb) : bs)
      | minb <= maxa = go $ (mina, max maxa maxb) : bs
      | otherwise = a : go bs

-- >>> solve 10 20 $ parse "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\nSensor at x=9, y=16: closest beacon is at x=10, y=16\nSensor at x=13, y=2: closest beacon is at x=15, y=3\nSensor at x=12, y=14: closest beacon is at x=10, y=16\nSensor at x=10, y=20: closest beacon is at x=10, y=16\nSensor at x=14, y=17: closest beacon is at x=10, y=16\nSensor at x=8, y=7: closest beacon is at x=2, y=10\nSensor at x=2, y=0: closest beacon is at x=2, y=10\nSensor at x=0, y=11: closest beacon is at x=2, y=10\nSensor at x=20, y=14: closest beacon is at x=25, y=17\nSensor at x=17, y=20: closest beacon is at x=21, y=22\nSensor at x=16, y=7: closest beacon is at x=15, y=3\nSensor at x=14, y=3: closest beacon is at x=15, y=3\nSensor at x=20, y=1: closest beacon is at x=15, y=3"
-- (26,52000011)

solve :: Int -> Int -> [Reading] -> (Int, Int)
solve y maxxy l = (p1, p2)
  where
    lineBeacons = S.fromList $ fst <$> filter ((== y) . snd) (rBeacon <$> l)
    p1 = S.size $ S.difference (go l S.empty) lineBeacons
    p2 = case findSeg 0 of
      (n, [(1, _)]) -> n
      (n, (_, x) : xs) -> n + 4000000 * x
      _ -> 0
    findSeg n
      | n == maxxy = (n, [])
      | otherwise = case joinSegs $ segment maxxy n <$> l of
        [(0, m)] -> findSeg $ n + 1
        (s : ss) -> (n, s : ss)
        _ -> (n, [(1, 2)])
    go bl imp = case bl of
      [] -> imp
      (r@(Reading se@(sx, sy) be) : rs) -> go rs $ S.union imp nx
        where
          mdis = manhattan se be
          ydis = abs $ y - sy
          dx = mdis - ydis
          nx = S.fromList [sx - dx .. sx + dx]

main :: IO ()
main = readFile "input/day15.txt" >>= print . solve 2000000 4000000 . parse
