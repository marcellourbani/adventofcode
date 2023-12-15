#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import Data.Map.Strict as M
import Data.Maybe (isNothing)

data RockType = Square | Round deriving (Show, Eq)

data GameMap = GameMap Int Int (M.Map (Int, Int) RockType) deriving (Show)

parse :: String -> GameMap
parse s = GameMap w h $ M.fromList gr
  where
    gr = [((x, y), readRock c) | (y, l) <- zip [0 ..] $ lines s, (x, c) <- zip [0 ..] l, c `elem` "#O"]
    readRock c = if c == '#' then Square else Round
    ls = lines s
    w = length $ head ls
    h = length ls

moveNorth :: GameMap -> GameMap
moveNorth gm@(GameMap h w g)
  | moved == roundk = gm
  | otherwise = moveNorth $ GameMap h w g'
  where
    canMove (GameMap _ _ g) (x, y)
      | y == 0 = False
      | isNothing $ M.lookup (x, y - 1) g = True
      | otherwise = False
    top g (x, y)
      | canMove g (x, y) = top g (x, y - 1)
      | otherwise = (x, y)
    (rounds, squares) = M.partition (== Round) g
    roundk = M.keys rounds
    moved = top gm <$> roundk
    g' = M.union squares $ M.fromList $ (,Round) <$> moved

weights (GameMap _ h g) = (h -) . snd <$> M.keys (M.filter (== Round) g)

-- >>> solve $ parse "O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#...."
-- (136,1)
solve l = (p1, p2)
  where
    p1 = sum $ weights $ moveNorth l
    p2 = 1

main :: IO ()
main = readFile "input/day14.txt" >>= print . solve . parse
