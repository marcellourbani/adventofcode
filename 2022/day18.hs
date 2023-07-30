#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import Data.List (sort)
import qualified Data.Set as S

parse :: String -> S.Set (Int, Int, Int)
parse s = S.fromList $ read <$> coords
  where
    coords = (<> ")") . ('(' :) <$> lines s

neighbors :: (Int, Int, Int) -> S.Set (Int, Int, Int)
neighbors (x, y, z) =
  S.fromList [(x + 1, y, z), (x - 1, y, z), (x, y + 1, z), (x, y - 1, z), (x, y, z + 1), (x, y, z - 1)]

exposed :: S.Set (Int, Int, Int) -> (Int, Int, Int) -> Int
exposed cubes cube = 6 - S.size (S.intersection cubes $ neighbors cube)

external :: S.Set (Int, Int, Int) -> Int
external voxels = go (S.singleton (-1, -1, -1)) S.empty 0
  where
    inbox (x, y, z) = and [x >= -1, x <= 22, y >= -1, y <= 22, z >= -1, z <= 22]
    go current visited tot =
      if S.null current
        then tot
        else go current' visited' tot'
      where
        valid c = S.notMember c visited && inbox c
        curneighbors = S.filter valid . neighbors <$> S.toList current
        newfaces = sum $ S.size . S.intersection voxels <$> curneighbors
        newcandidates = S.unions curneighbors
        onedge = voxels `S.intersection` newcandidates
        current' = newcandidates `S.difference` onedge
        visited' = visited `S.union` current'
        tot' = tot + newfaces

-- >>> solve $ parse "2,2,2\n1,2,2\n3,2,2\n2,1,2\n2,3,2\n2,2,1\n2,2,3\n2,2,4\n2,2,6\n1,2,5\n3,2,5\n2,1,5\n2,3,5"
-- (64,58)

solve :: S.Set (Int, Int, Int) -> (Int, Int)
solve l = (p1, p2)
  where
    cubes = S.toList l
    p1 = sum $ exposed l <$> cubes
    p2 = external l

main :: IO ()
main = readFile "input/day18.txt" >>= print . solve . parse
