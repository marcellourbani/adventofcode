#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import Data.Char (isDigit)
import Data.List (partition, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

data GameMap = GameMap {maxx :: Int, maxy :: Int, grid :: M.Map (Int, Int) Char} deriving (Show, Eq, Ord)

parse :: String -> GameMap
parse s = GameMap mx my m
  where
    m = M.fromList [((x, y), c) | (y, l) <- zip [0 ..] $ lines s, (x, c) <- zip [0 ..] l, c /= '.']
    mx = maximum $ fst <$> M.keys m
    my = maximum $ snd <$> M.keys m

mapAt :: GameMap -> Int -> Int -> Char
mapAt m x y = fromMaybe '.' $ grid m M.!? (x, y)

numberAt :: GameMap -> (Int, Int) -> ((Int, Int), Int)
numberAt m (x, y) = ((startx, y), num)
  where
    movex f x'
      | isDigit $ mapAt m (f x') y = movex f (f x')
      | otherwise = x'
    startx = movex (+ (-1)) x
    endx = movex (+ 1) x
    num = read $ mapAt m <$> [startx .. endx] <*> [y]

-- >>> solve $ parse "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.."
-- (4361,467835)
solve :: GameMap -> (Int, Int)
solve l = (p1, p2)
  where
    symbols = M.filter (not . isDigit) $ grid l
    gears = M.filter (== '*') $ grid l
    mapAt' x y = ((x, y), mapAt l x y)
    neighbors (x, y) = filter (isDigit . snd) $ mapAt' <$> [x - 1 .. x + 1] <*> [y - 1 .. y + 1]
    neighNum (x, y) = S.fromList $ numberAt l . fst <$> neighbors (x, y)
    gearRatio p = case S.toList $ neighNum p of
      [(_, a), (_, b)] -> a * b
      _ -> 0
    p1 = sum $ snd <$> (M.keys symbols >>= S.toList . neighNum)
    p2 = sum $ gearRatio <$> M.keys gears

main :: IO ()
main = readFile "input/day03.txt" >>= print . solve . parse
