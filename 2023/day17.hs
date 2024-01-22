#!/usr/bin/env stack
-- stack --resolver lts-18.18 script --optimize

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.PQueue.Prio.Min as P
import qualified Data.Set as S

type Vector = (Int, Int)

data GameMap = GameMap {gW :: Int, gH :: Int, gMap :: M.Map Vector Char} deriving (Eq)

data State = State {sPos :: Vector, sSpeed :: Vector, sLen :: Int} deriving (Show, Eq, Ord)

mapTile :: GameMap -> Vector -> Int
mapTile gm p = read $ (: "") <$> fromMaybe '0' $ M.lookup p $ gMap gm

instance Show GameMap where
  show gm@(GameMap w h m) = unlines [[head $ show (mapTile gm (x, y)) | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]

parse :: String -> GameMap
parse s = GameMap (length $ head ls) (length ls) m
  where
    m = M.fromList [((x, y), c) | (y, l) <- zip [0 ..] ls, (x, c) <- zip [0 ..] l]
    ls = lines s

inMap :: GameMap -> Vector -> Bool
inMap (GameMap w h _) (x, y) = x >= 0 && x < w && y >= 0 && y < h

addVector :: Vector -> Vector -> Vector
addVector (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

nextStates :: GameMap -> State -> [State]
nextStates gm s@(State p v l) = filter (inMap gm . sPos) $ mapMaybe ns vs
  where
    vs = ((0,) <$> [1, -1]) <> ((,0) <$> [1, -1])
    ns v1@(x, y)
      | (-x, -y) == v = Nothing
      | v1 == v && l == 3 = Nothing
      | v1 == v = Just $ State (addVector p v) v $ l + 1
      | otherwise = Just $ State (addVector p v1) v1 1

minPath :: GameMap -> Int
minPath gamemap@(GameMap w h _) = go 0 initial initial
  where
    initial = M.singleton (State (0, 0) (0, 0) 0) 0
    isFinal s = sPos s == (w - 1, h - 1)
    findFinal s = S.filter isFinal $ M.keysSet s
    addValue c p = (p, c + mapTile gamemap (sPos p))
    go n costs frontier =
      if M.null frontier
        then minimum $ M.restrictKeys costs $ findFinal costs
        else go (n + 1) costs' frontier'
      where
        improved k v = M.findWithDefault maxBound k costs > v
        nexts (p, c) = M.fromList $ addValue c <$> nextStates gamemap p
        newCandidates = M.unionsWith min $ nexts <$> M.toList frontier
        frontier' = M.filterWithKey improved newCandidates
        costs' = M.unionWith min costs frontier'

nextStates2 :: GameMap -> Int -> Int -> State -> [State]
nextStates2 gm mind maxd s@(State p v l) = states
  where
    follow = [State (addVector p v) v (l + 1) | l < maxd, fst v /= 0 || snd v /= 0]
    turnedy =
      if fst v == 0 then [State (addVector p (mind, 0)) (1, 0) mind, State (addVector p (-mind, 0)) (-1, 0) mind] else []
    turnedx = if snd v == 0 then [State (addVector p (0, mind)) (0, 1) mind, State (addVector p (0, -mind)) (0, -1) mind] else []
    states = filter (inMap gm . sPos) $ follow <> turnedx <> turnedy

moveCost :: GameMap -> State -> State -> Int
moveCost gm (State p1 _ _) (State p2 _ _) = sum $ go p1 p2
  where
    go (x1, y1) (x2, y2) =
      mapTile gm <$> case (compare x2 x1, compare y2 y1) of
        (EQ, EQ) -> []
        (EQ, LT) -> (x1,) <$> [y2 .. y1 - 1]
        (EQ, GT) -> (x1,) <$> [y1 + 1 .. y2]
        (LT, EQ) -> (,y1) <$> [x2 .. x1 - 1]
        (GT, EQ) -> (,y1) <$> [x1 + 1 .. x2]

-- Dijkstra's algorithm
shortestPath ::
  (Num n, Ord n, Ord state) =>
  state ->
  (state -> [state]) ->
  (state -> state -> n) ->
  (state -> Bool) ->
  Maybe (n, [state])
shortestPath initial nexts cost isGoal = case go M.empty iq M.empty of
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
          dists s = (curdist + cost cur s, (s, Just cur))
          newentries = dists <$> filter (`M.notMember` nodes) (nexts cur)
          queue' = P.union queue $ P.fromList newentries
          nodes' = M.insert cur curdist nodes
          prevs' = case prev of
            Just p -> M.insert cur p prevs
            _ -> prevs

-- >>> solve $ parse "2413432311323\n3215453535623\n3255245654254\n3446585845452\n4546657867536\n1438598798454\n4457876987766\n3637877979653\n4654967986887\n4564679986453\n1224686865563\n2546548887735\n4322674655533"
-- (102,94)

solve :: GameMap -> (Int, Int)
solve gm@(GameMap w h _) = (p1, p2)
  where
    initial = State (0, 0) (0, 0) 0
    goal = (w - 1, h - 1)
    isGoal = (== goal) . sPos
    p1 = maybe 0 fst $ shortestPath initial (nextStates2 gm 1 3) (moveCost gm) isGoal
    p2 = maybe 0 fst $ shortestPath initial (nextStates2 gm 4 10) (moveCost gm) isGoal

main :: IO ()
main = readFile "input/day17.txt" >>= print . solve . parse
