#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import Data.List (nub)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

data GameMap c = GameMap {gmW :: Int, gmH :: Int, gmMap :: M.Map (Int, Int) c} deriving (Eq)

instance Functor GameMap where
  fmap f (GameMap w h m) = GameMap w h $ M.map f m

instance Show (GameMap Char) where
  show gm@(GameMap w h m) = unlines [[mapTile '.' gm (x, y) | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]

mapTile :: c -> GameMap c -> (Int, Int) -> c
mapTile def gm p = fromMaybe def $ M.lookup p $ gmMap gm

inMap :: GameMap c -> (Int, Int) -> Bool
inMap (GameMap w h _) (x, y) = 0 <= x && x < w && 0 <= y && y < h

parse :: String -> GameMap Char
parse s = GameMap w h $ M.fromList [((x, y), c) | (y, line) <- zip [0 ..] l, (x, c) <- zip [0 ..] line, c /= '.']
  where
    l = lines s
    w = length $ head l
    h = length l

reachable :: GameMap Char -> Int -> [(Int, Int)] -> [(Int, Int)]
reachable gm n l
  | n == 0 = l
  | otherwise = reachable gm (n - 1) l'
  where
    neighs (x, y) = ((x,) <$> [y - 1, y + 1]) <> ((,y) <$> [x - 1, x + 1])
    candidates = l >>= neighs
    l' = nub $ filter available $ filter (inMap gm) candidates
    available p = mapTile '.' gm p /= '#'

-- >>> solve 6 $ parse "...........\n.....###.#.\n.###.##..#.\n..#.#...#..\n....#.#....\n.##..S####.\n.##..#...#.\n.......##..\n.##.#.####.\n.##..##.##.\n..........."
-- 16
solve n gm = p1
  where
    seed = head $ M.keys $ M.filter (== 'S') $ gmMap gm
    p1 = length $ reachable gm n [seed]

main :: IO ()
main = readFile "input/day21.txt" >>= print . solve 64 . parse
