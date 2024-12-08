#!/usr/bin/env stack
-- stack --resolver lts-20.26 script --optimize

{-# LANGUAGE PackageImports #-}

module Main where

import Data.List (elemIndex, findIndex, nub)
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

rot90 :: (Num a) => V2 a -> V2 a -- 90^ right, with y going down
rot90 (V2 a b) = V2 (-b) a

findPath :: GameMap Char -> (V2 Int, V2 Int) -> (Bool, [(V2 Int, V2 Int)])
findPath i (startp, startV) = go S.empty [] startp startV
  where
    go prevs visited pos v
      | not (inMap i pos) = (False, reverse visited)
      | S.member (pos, v) prevs = (True, reverse visited)
      | c == '#' = go prevs' visited' pos v'
      | otherwise = go prevs' visited' pos' v
      where
        visited' = (pos, v) : visited
        prevs' = S.insert (pos, v) prevs
        pos' = pos + v
        c = mapTile '.' i pos'
        v' = rot90 v

drawPath :: GameMap a -> a -> [V2 Int] -> GameMap a
drawPath gm tile path = gm {gmMap = M.union (gmMap gm) $ M.fromList (map (,tile) path)}

part1 :: Input -> Int
part1 i = length $ nub $ fst <$> snd (findPath i (startp, startV))
  where
    startp = head $ M.keys $ M.filter (== '^') (gmMap i)
    startV = V2 0 (-1)

part2 :: Input -> Int
part2 i = length $ nub $ go path
  where
    startp = head $ M.keys $ M.filter (== '^') (gmMap i)
    startV = V2 0 (-1)
    path = snd $ findPath i (startp, startV)
    obstruct p = i {gmMap = M.insert p '#' $ gmMap i}
    obstructions = go path
    go p = case p of
      a@(p1, _) : b@(p2, _) : ps
        | p1 == p2 -> nexts
        | obstructed -> [fst b] : nexts
        | otherwise -> nexts
        where
          i' = obstruct $ fst b
          obstructed = fst $ findPath i' (startp, startV)
          nexts = go $ b : ps
      _ -> []

-- >>> solve $ parse "....#.....\n.........#\n..........\n..#.......\n.......#..\n..........\n.#..^.....\n........#.\n#.........\n......#..."
-- (41,6)

solve l = (p1, p2)
  where
    p1 = part1 l
    p2 = part2 l

main :: IO ()
main = readFile "input/day06.txt" >>= print . solve . parse
