#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import Data.Foldable (foldl')
import qualified Data.Map.Strict as M

data Direction = L | R deriving (Show, Eq, Read)

data Game = Game {gDir :: [Direction], gRules :: M.Map String (String, String)} deriving (Show, Eq)

parse :: String -> Game
parse s = Game (read . (: "") <$> h) $ M.fromList (toTuple . words <$> lf)
  where
    h : _ : ls = lines s
    lf = filter (`notElem` "=(),") <$> ls
    toTuple [a, b, c] = (a, (b, c))

nextLabel :: Game -> String -> Direction -> String
nextLabel (Game _ rs) l d = case M.lookup l rs of
  Nothing -> undefined
  Just (a, b)
    | d == L -> a
    | d == R -> b

-- >>> solve $ parse "RL\n\nAAA = (BBB, CCC)\nBBB = (DDD, EEE)\nCCC = (ZZZ, GGG)\nDDD = (DDD, DDD)\nEEE = (EEE, EEE)\nGGG = (GGG, GGG)\nZZZ = (ZZZ, ZZZ)"
-- >>> solve $ parse "LLR\n\nAAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\nZZZ = (ZZZ, ZZZ)"
-- (2,2)
-- (6,6)
-- Prelude.undefined
solve :: Game -> (Int, Int)
solve g@(Game ds gm) = (p1, p2)
  where
    p2 = foldl' lcm 1 cycles
    cycles = go2 (cycle ds) <$> startlabels
    startlabels = filter ((== 'A') . last) $ M.keys gm
    p1 = go "AAA" $ cycle ds
    go2 (d : ds) l
      | last l == 'Z' = 0
      | otherwise = 1 + go2 ds (nextLabel g l d)
    go l (d : ds)
      | l == "ZZZ" = 0
      | otherwise = 1 + go (nextLabel g l d) ds

main :: IO ()
main = readFile "input/day08.txt" >>= print . solve . parse
