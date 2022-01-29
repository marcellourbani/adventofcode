#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE TupleSections #-}

module Main where

import Data.Foldable (Foldable (foldl'))
import qualified Data.Map.Strict as M

parse :: String -> [Bool]
parse s = (== '.') <$> s

isSafe :: [Bool] -> Bool
isSafe pat = case pat of
  [False, False, True] -> False
  [True, False, False] -> False
  [False, True, True] -> False
  [True, True, False] -> False
  _ -> True

createMap :: [Bool] -> Int -> M.Map (Int, Int) Bool
createMap first lines = M.filter id $ foldl' addline firstlinemap [2 .. lines]
  where
    firstlinemap = M.fromList $ zip (zip [1 ..] $repeat 1) first
    wid = length first
    prevSafe m (x, y) = isSafe $ M.findWithDefault True . (,y -1) <$> [x -1 .. x + 1] <*> pure m
    addline m l = M.union m $ M.fromList newline
      where
        cs = zip [1 .. wid] $ repeat l
        newline = zip cs $ prevSafe m <$> cs

-- creating a 4000000 line map was fast enough, but this is better
-- would probably be even faster using lists as we always scan sequentially
countSafe :: [Bool] -> Int -> Int
countSafe first lines = go firstlinemap 1
  where
    firstlinemap = M.fromList $ zip [1 ..] first
    wid = length first
    prevSafe m x = isSafe $ M.findWithDefault True <$> [x -1 .. x + 1] <*> pure m
    go l i
      | i == lines = cur
      | otherwise = cur + go next (i + 1)
      where
        cur = M.size $ M.filter id l
        next = M.fromList $ zip [1 .. wid] $ prevSafe l <$> [1 .. wid]

-- >>> length $ createMap (parse ".^^.^.^^^^") 10
-- 38

solve :: [Bool] -> (Int, Int)
solve l = (length $ createMap l 40, countSafe l 400000)

main :: IO ()
main = readFile "input/day18.txt" >>= print . solve . parse
