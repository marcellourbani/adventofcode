#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List (sort)
import qualified Data.Set as S

parse :: String -> S.Set (Int, Int, Int)
parse s = S.fromList $ read <$> coords
  where
    coords = (<> ")") . ('(' :) <$> lines s

neighbors :: (Int, Int, Int) -> S.Set (Int, Int, Int)
neighbors (x, y, z) = S.fromList [(x + 1, y, z), (x -1, y, z), (x, y + 1, z), (x, y -1, z), (x, y, z + 1), (x, y, z -1)]

exposed :: S.Set (Int, Int, Int) -> (Int, Int, Int) -> Int
exposed cubes cube = 6 - S.size (S.intersection cubes $ neighbors cube)

external :: S.Set (Int, Int, Int) -> Int
external cubes = 2 * (top + front + side)
  where
    lcubes = S.toList cubes
    cx (x, _, _) = x
    cy (_, y, _) = y
    cz (_, _, z) = z
    lim f = (f $ cy <$> lcubes, f $ cy <$> lcubes, f $ cz <$> lcubes)
    (minx, miny, minz) = lim minimum
    (maxx, maxy, maxz) = lim maximum

    has f = not $ S.null $ S.filter f cubes
    topfilt (x, y) (x', y', _) = x == x' && y == y'
    top = length $ filter id $ has . topfilt <$> [(a, b) | a <- [minx .. maxx], b <- [miny .. maxy]]
    sidefilt (y, z) (_, y', z') = z == z' && y == y'
    side = length $ filter id $ has . sidefilt <$> [(a, b) | a <- [miny .. maxy], b <- [minz .. maxz]]
    frontfilt (x, z) (x', _, z') = z == z' && x == x'
    front = length $ filter id $ has . frontfilt <$> [(a, b) | a <- [minx .. maxx], b <- [minz .. maxz]]

-- >>> solve $ parse "2,2,2\n1,2,2\n3,2,2\n2,1,2\n2,3,2\n2,2,1\n2,2,3\n2,2,4\n2,2,6\n1,2,5\n3,2,5\n2,1,5\n2,3,5"
-- (64,50)

solve :: S.Set (Int, Int, Int) -> (Int, Int)
solve l = (p1, p2)
  where
    cubes = S.toList l
    p1 = sum $ exposed l <$> cubes
    p2 = external l

main :: IO ()
main = readFile "input/day18.txt" >>= print . solve . parse
