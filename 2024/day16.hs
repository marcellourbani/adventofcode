#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

{-# LANGUAGE PackageImports #-}

module Main where

import Control.Lens ((^.))
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, fromMaybe)
import Data.PQueue.Prio.Min qualified as P
import Linear.V2
import "containers" Data.Map.Strict qualified as M
import "containers" Data.Set qualified as S

data State = State {stLoc :: V2 Int, stDir :: V2 Int} deriving (Show, Eq, Ord)

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

transitions :: GameMap Char -> State -> [(State, Int)]
transitions gm (State l d)
  | nt == '#' = rots
  | otherwise = (State (l + d) d, 1) : rots
  where
    nt = mapTile '.' gm $ l + d
    rots = [(State l $ rot90l d, 1000), (State l $ rot90r d, 1000)]
    rot90r (V2 a b) = V2 (-b) a
    rot90l (V2 a b) = V2 b (-a)

drawPath :: GameMap c -> M.Map (V2 Int) c -> GameMap c
drawPath gm m = gm {gmMap = M.union (gmMap gm) m}

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

part1 :: Input -> Int
part1 l = cost
  where
    nm = drawPath l $ M.fromList $ (,'O') . stLoc <$> path -- graphical output
    (cost, path) = fromJust $ shortestPath initial (transitions l) isGoal
    sl = head $ M.keys $ M.filter (== 'S') $ gmMap l
    initial = State sl $ V2 1 0
    isGoal (State p _) = mapTile '.' l p == 'E'

-- part2 :: Input -> Int
part2 s = 0

-- >>> solve $ parse "###############\n#.......#....E#\n#.#.###.#.###.#\n#.....#.#...#.#\n#.###.#####.#.#\n#.#.#.......#.#\n#.#.#####.###.#\n#...........#.#\n###.#.#####.#.#\n#...#.....#.#.#\n#.#.#.###.#.#.#\n#.....#...#.#.#\n#.###.#.#.#.#.#\n#S..#.....#...#\n###############"
-- (7036,0)

solve l = (p1, p2)
  where
    p1 = part1 l
    p2 = part2 l

main :: IO ()
main = readFile "input/day16.txt" >>= print . solve . parse
