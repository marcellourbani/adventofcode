#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

{-# LANGUAGE PackageImports #-}

module Main where

import Data.List (filter, nub)
import Data.Maybe (fromMaybe)
import Linear.V2
import "containers" Data.Map.Strict qualified as M
import "containers" Data.Set qualified as S

data GameMap c = GameMap {gmW :: Int, gmH :: Int, gmMap :: M.Map (V2 Int) c} deriving (Eq)

instance Functor GameMap where
  fmap f (GameMap w h m) = GameMap w h $ M.map f m

instance Show (GameMap Char) where
  show gm@(GameMap w h m) = unlines [[mapTile '.' gm (V2 x y) | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]

instance {-# OVERLAPPABLE #-} (Show c) => Show (GameMap c) where
  show gm@(GameMap w h m) = show $ gm {gmMap = head . show <$> m}

mapTile :: c -> GameMap c -> V2 Int -> c
mapTile def gm p = fromMaybe def $ M.lookup p $ gmMap gm

inMap :: GameMap c -> V2 Int -> Bool
inMap (GameMap w h _) (V2 x y) = 0 <= x && x < w && 0 <= y && y < h

parse :: String -> GameMap Char
parse s = GameMap w h $ M.fromList [(V2 x y, c) | (y, line) <- zip [0 ..] l, (x, c) <- zip [0 ..] line, c /= '.']
  where
    l = lines s
    w = length $ head l
    h = length l

type Input = GameMap Char

part1 :: Input -> Int
part1 m@(GameMap _ _ gm) = S.size $ S.unions $ go <$> antennas
  where
    antennas = nub $ snd <$> M.toAscList gm
    go a = S.fromList nodes
      where
        coords = M.keys $ M.filter (== a) gm
        nodes = [n | a <- coords, b <- coords, a < b, let n1 = b + b - a, let n2 = a + a - b, n <- [n1, n2], inMap m n]

line :: Input -> V2 Int -> V2 Int -> S.Set (V2 Int)
line gm pos (V2 vx vy) = S.fromList $ points pos step <> points pos (-step)
  where
    scale = gcd vx vy
    step = V2 (div vx scale) (div vy scale)
    l = S.fromList $ points pos step <> points pos (-step)
    points p v
      | inMap gm p = p : points (p + v) v
      | otherwise = []

part2 :: Input -> Int
part2 m@(GameMap _ _ gm) = S.size $ S.unions $ go <$> antennas
  where
    antennas = nub $ snd <$> M.toAscList gm
    go a = S.fromList nodes
      where
        coords = M.keys $ M.filter (== a) gm
        nodes = [n | a <- coords, b <- coords, a < b, n <- S.toList $ line m a (b - a), inMap m n]

-- >>> solve $ parse "............\n........0...\n.....0......\n.......0....\n....0.......\n......A.....\n............\n............\n........A...\n.........A..\n............\n............"
-- (14,34)

solve :: Input -> (Int, Int)
solve l = (p1, p2)
  where
    p1 = part1 l
    p2 = part2 l

main :: IO ()
main = readFile "input/day08.txt" >>= print . solve . parse
