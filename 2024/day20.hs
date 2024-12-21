#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

{-# LANGUAGE PackageImports #-}

module Main where

import Control.Lens ((^.))
import Data.Maybe (fromMaybe)
import Data.PQueue.Prio.Min qualified as P
import Linear.V2
import "containers" Data.Map.Strict qualified as M
import "containers" Data.Set qualified as S

data GameMap c = GameMap {gmW :: Int, gmH :: Int, gmMap :: M.Map (V2 Int) c} deriving (Eq)

instance Functor GameMap where
  fmap f (GameMap w h m) = GameMap w h $ M.map f m

instance Show (GameMap Char) where
  show gm@(GameMap w h m) = unlines [[mapTile '.' gm (V2 x y) | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]

mapTile :: c -> GameMap c -> V2 Int -> c
mapTile def gm p = fromMaybe def $ M.lookup p $ gmMap gm

inMap :: GameMap c -> V2 Int -> Bool
inMap (GameMap w h _) (V2 x y) = 0 <= x && x < w && 0 <= y && y < h

drawPath :: Bool -> GameMap c -> M.Map (V2 Int) c -> GameMap c
drawPath drawover gm m = gm {gmMap = if drawover then M.union m (gmMap gm) else M.union (gmMap gm) m}

keysOf :: (Eq c) => GameMap c -> c -> [V2 Int]
keysOf gm g = M.keys $ M.filter (== g) $ gmMap gm

parse :: String -> GameMap Char
parse s = GameMap w h $ M.fromList [(V2 x y, c) | (y, line) <- zip [0 ..] l, (x, c) <- zip [0 ..] line, c /= '.']
  where
    l = lines s
    w = length $ head l
    h = length l

shortestPath :: (Num n, Ord n, Ord state) => state -> ((state, n) -> [(state, n)]) -> (state -> Bool) -> Maybe (n, [state])
shortestPath initial nexts isGoal = case go M.empty iq M.empty of
  Nothing -> Nothing
  Just (target, dist, prevs) -> Just (dist, target : findpath prevs target)
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
          newentries = dists <$> filter ((`M.notMember` nodes) . fst) (nexts (cur, curdist))
          queue' = P.union queue $ P.fromList newentries
          nodes' = M.insert cur curdist nodes
          prevs' = case prev of
            Just p -> M.insert cur p prevs
            _ -> prevs

directions :: [V2 Int]
directions = (V2 0 <$> [-1, 1]) <> (V2 <$> [-1, 1] <*> [0])

findPath :: GameMap Char -> [V2 Int]
findPath m = maybe [] snd $ shortestPath startP nexts isGoal
  where
    nexts (k, _) = [(n, 1) | d <- directions, let n = d + k, mapTile '.' m n /= '#']
    endP = head $ keysOf m 'E'
    startP = head $ keysOf m 'S'
    isGoal k = k == endP

betweenV2 :: V2 Int -> V2 Int -> [V2 Int]
betweenV2 (V2 x1 y1) (V2 x2 y2)
  | x1 == x2 = V2 x1 <$> [min y1 y2 + 1 .. max y1 y2 - 1]
  | otherwise = V2 <$> [min x1 x2 + 1 .. max x1 x2 - 1] <*> [y1]

part1 :: Int -> GameMap Char -> [V2 Int] -> Int
part1 mincheat m baseSol = length cheats
  where
    nm = drawPath True m $ M.fromList $ (,'o') <$> baseSol
    isShortcut (v1, n1) v2 = case (mandist v1 v2, M.lookup v2 spath) of
      (d, Just n2) | d >= 2 && n1 - n2 - d >= mincheat -> True
      _ -> False
    cheats = S.toList $ S.fromList $ [walls s ns | (s, p) <- M.toList spath, dp <- candVecs, let ns = s + dp, isShortcut (s, p) ns]

    candVecs = (*) <$> [2, 3] <*> directions
    mandist (V2 x1 y1) (V2 x2 y2) = abs (x1 - x2) + abs (y1 - y2)
    spath = M.fromList $ zip baseSol [0 ..]
    walls v1 v2 = filter ((== '#') . mapTile '.' m) $ betweenV2 v1 v2

-- part2 :: Input -> Int
part2 l = 0

-- >>> solve 38 $ parse "###############\n#...#...#.....#\n#.#.#.#.#.###.#\n#S#...#.#.#...#\n#######.#.#.###\n#######.#.#...#\n#######.#.###.#\n###..E#...#...#\n###.#######.###\n#...###...#...#\n#.#####.#.###.#\n#.#...#.#.#...#\n#.#.#.#.#.#.###\n#...#...#...###\n###############"
-- (7,0)

solve n m = (p1, p2)
  where
    path = findPath m
    p1 = part1 n m path
    p2 = part2 m

main :: IO ()
main = readFile "input/day20.txt" >>= print . solve 100 . parse
