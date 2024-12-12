#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

{-# LANGUAGE PackageImports #-}

module Main where

import Data.Maybe (fromMaybe)
import Data.PQueue.Prio.Min qualified as P
import Linear.V2
import "containers" Data.Map.Strict qualified as M
import "containers" Data.Set qualified as S

data GameMap c = GameMap {gmW :: Int, gmH :: Int, gmMap :: M.Map (V2 Int) c} deriving (Eq)

instance Functor GameMap where
  fmap f (GameMap w h m) = GameMap w h $ M.map f m

instance Show (GameMap Int) where
  show gm@(GameMap w h m) = unlines [[tileChar $ mapTile (-1) gm (V2 x y) | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]
    where
      tileChar n | n < 0 = '.' | otherwise = head $ show n

instance {-# OVERLAPPABLE #-} (Show c) => Show (GameMap c) where
  show gm@(GameMap w h m) = show $ gm {gmMap = head . show <$> m}

mapTile :: c -> GameMap c -> V2 Int -> c
mapTile def gm p = fromMaybe def $ M.lookup p $ gmMap gm

inMap :: GameMap c -> V2 Int -> Bool
inMap (GameMap w h _) (V2 x y) = 0 <= x && x < w && 0 <= y && y < h

parse :: String -> GameMap Int
parse s = GameMap w h $ M.fromList [(V2 x y, read c) | (y, line) <- zip [0 ..] l, (x, c) <- zip [0 ..] line, c /= "."]
  where
    l = map (: []) <$> lines s
    w = length $ head l
    h = length l

directions = [V2 0, flip V2 0] <*> [-1, 1]

part1 :: GameMap Int -> Int
part1 i@(GameMap _ _ gm) = sum $ length . S.fromList . go <$> starts
  where
    starts = M.keys $ M.filter (== 0) gm
    at = mapTile (-1) i
    nexts p = filter ((== (at p + 1)) . at) ((p +) <$> directions)
    go key
      | at key == 9 = [key]
      | otherwise = nexts key >>= go

part2 :: GameMap Int -> Int
part2 i@(GameMap _ _ gm) = length (starts >>= go)
  where
    starts = M.keys $ M.filter (== 0) gm
    at = mapTile (-1) i
    nexts p = filter ((== (at p + 1)) . at) ((p +) <$> directions)
    go key
      | at key == 9 = [[key]]
      | otherwise = (key :) <$> (nexts key >>= go)

-- >>> solve $ parse "0123\n1234\n8765\n9876"
-- >>> solve $ parse "89010123\n78121874\n87430965\n96549874\n45678903\n32019012\n01329801\n10456732"
-- (1,16)
-- (36,81)

solve :: GameMap Int -> (Int, Int)
solve l = (p1, p2)
  where
    p1 = part1 l
    p2 = part2 l

main :: IO ()
main = readFile "input/day10.txt" >>= print . solve . parse
