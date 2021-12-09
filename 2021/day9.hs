#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S

type Input = M.Map (Int, Int) Int

parse :: String -> Input
parse s = M.fromList points
  where
    m = map (read . pure) <$> lines s
    points = [((x, y), v) | (y, l) <- zip [0 ..] m, (x, v) <- zip [0 ..] l]

-- >>> solve $ parse "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"
-- (15,0)

solve :: Input -> (Int, Int)
solve i = (sum risks, 0)
  where
    adjscoord (x, y) = ((,y) <$> [x -1, x + 1]) <> ((x,) <$> [y - 1, y + 1])
    adjacents p = catMaybes $ M.lookup <$> adjscoord p <*> pure i
    isMin (c, v) = all (> v) $ adjacents c
    lowPoints = filter isMin $ M.toList i
    risks = (+ 1) . snd <$> lowPoints

main :: IO ()
main = readFile "input/day9.txt" >>= print . solve . parse
