#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

{-# LANGUAGE PackageImports #-}

module Main where

import Data.List.Split (splitOn)
import Data.Maybe (fromJust, fromMaybe)
import Data.PQueue.Prio.Min qualified as P
import Linear.V2
import "containers" Data.Map.Strict qualified as M

data GameMap c = GameMap {gmW :: Int, gmH :: Int, gmMap :: M.Map (V2 Int) c} deriving (Eq)

instance Functor GameMap where
  fmap f (GameMap w h m) = GameMap w h $ M.map f m

instance Show (GameMap Char) where
  show gm@(GameMap w h m) = unlines [[mapTile '.' gm (V2 x y) | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]

type Input = [V2 Int]

mapTile :: c -> GameMap c -> V2 Int -> c
mapTile def gm p = fromMaybe def $ M.lookup p $ gmMap gm

inMap :: GameMap c -> V2 Int -> Bool
inMap (GameMap w h _) (V2 x y) = 0 <= x && x < w && 0 <= y && y < h

drawPath :: GameMap c -> M.Map (V2 Int) c -> GameMap c
drawPath gm m = gm {gmMap = M.union (gmMap gm) m}

parse :: String -> Input
parse s = pl <$> lines s
  where
    tp [a, b] = V2 (read a) (read b)
    pl l = tp $ splitOn "," l

shortestPath :: (Num n, Ord n, Ord state) => state -> (state -> [(state, n)]) -> (state -> Bool) -> Maybe (n, [state])
shortestPath initial nexts isGoal = case go M.empty iq M.empty of
  Nothing -> Nothing
  Just (target, dist, prevs) -> Just (dist, findpath prevs target)
  where
    iq = P.singleton 0 (initial, Nothing)
    findpath prevs goal = case prevs M.!? goal of
      Nothing -> [goal]
      Just cur -> cur : findpath prevs cur
    go nodes queue prevs = case P.getMin queue of
      Nothing -> Nothing
      Just (_, (cur, _)) | M.member cur nodes -> go nodes (P.deleteMin queue) prevs
      Just (curdist, (cur, prev))
        | isGoal cur -> Just (cur, curdist, prevs')
        | otherwise -> go nodes' queue' prevs'
        where
          dists (s, cost) = (curdist + cost, (s, Just cur))
          newentries = dists <$> filter ((`M.notMember` nodes) . fst) (nexts cur)
          queue' = P.union queue $ P.fromList newentries
          nodes' = M.insert cur curdist nodes
          prevs' = case prev of
            Just p -> M.insert cur p prevs
            _ -> prevs

directions :: [V2 Int]
directions = (V2 0 <$> [1, -1]) <> (V2 <$> [1, -1] <*> [0])

createMap :: Int -> [V2 Int] -> GameMap Char
createMap width bytes = GameMap width width $ M.fromList $ (,'#') <$> bytes

solveMap :: (Num n, Ord n) => GameMap Char -> Maybe (n, [V2 Int])
solveMap m@(GameMap w h gm) = shortestPath initial nexts isGoal
  where
    initial = V2 0 0
    valid p = inMap m p && mapTile '.' m p /= '#'
    nexts p = (,1) <$> filter valid ((p +) <$> directions)
    goal = V2 (w - 1) (h - 1)
    isGoal g = g == goal

part1 :: Int -> Int -> Input -> Int
part1 width maxb l = fst . fromJust $ solveMap $ createMap width $ take maxb l

part2 :: Int -> Int -> [V2 Int] -> V2 Int
part2 width minb l = go maxb minb $ mid maxb minb
  where
    maxb = length l - 1
    sm le = solveMap $ createMap width $ take le l
    mid a b = div (a + b) 2
    go mab mib cur = case (sm cur, (mab == mib) || (mab == mib + 1)) of
      (Nothing, True) -> l !! (cur - 1)
      (Nothing, False) -> go cur mib $ mid cur mib
      (_, True) -> l !! (mab - 1)
      _ -> go mab cur $ mid cur mab

-- >>> solve 7 12 $ parse "5,4\n4,2\n4,5\n3,0\n2,1\n6,3\n2,4\n1,5\n0,6\n3,3\n2,6\n5,1\n1,2\n5,5\n2,5\n6,5\n1,4\n0,4\n6,4\n1,1\n6,1\n1,0\n0,5\n1,6\n2,09"
-- (22,V2 6 1)

solve :: Int -> Int -> [V2 Int] -> (Int, V2 Int)
solve width maxb l = (p1, p2)
  where
    p1 = part1 width maxb l
    p2 = part2 width maxb l

main :: IO ()
main = readFile "input/day18.txt" >>= print . solve 71 1024 . parse
