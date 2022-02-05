#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE TupleSections #-}

module Main where

import Data.List (find)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.PQueue.Prio.Min as P
import qualified Data.Set as S

data Node = Node {used :: Int, avail :: Int} deriving (Show, Eq, Ord)

data State = State {smx :: Int, smy :: Int, sgoal :: (Int, Int), sempty :: (Int, Int), walls :: S.Set (Int, Int)} deriving (Show, Eq, Ord)

type Move = ((Int, Int), (Int, Int))

parse :: String -> [((Int, Int), Node)]
parse s = pl <$> drop 2 (lines s)
  where
    pl s = ((x, y), Node usd avl)
      where
        (r : re) = words . drop 16 . filter (`notElem` "T%") $s
        [x, y] = read <$> splitOn "-y" r
        [_, usd, avl, _] = read <$> re

aStar :: Ord s => P.MinPQueue Int (s, Int, [m]) -> S.Set s -> (s -> Bool) -> (s -> Int -> [(Int, (s, Int, m))]) -> Maybe (s, Int, [m])
aStar queue visited goal nextStates
  | P.null queue = Nothing
  | goal curst = Just (curst, curcost, curpath)
  | S.member curst visited = aStar queue' visited goal nextStates
  | otherwise = aStar queue'' visited' goal nextStates
  where
    (_, (curst, curcost, curpath)) = P.findMin queue
    queue' = P.deleteMin queue
    visited' = S.insert curst visited
    nexts l = case l of
      [] -> []
      (e, (s, c, m)) : xs
        | S.member s visited' -> nexts xs
        | otherwise -> (e, (s, c, curpath <> [m])) : nexts xs
    queue'' = P.union queue' $ P.fromList $ nexts $ nextStates curst curcost

isGoal :: State -> Bool
isGoal (State _ _ (x, y) _ _) = x == 0 && y == 0

sCost :: State -> Int
sCost s@(State _ _ (x, y) (hx, hy) _) = (x + y) + holedist
  where
    holedist = abs (hx - x) + abs (hy - y)

moveState :: State -> Move -> State
moveState (State x y p h w) (p1, p2) = State x y p' p1 w
  where
    p' = if p == p1 then p2 else p

validMoves :: State -> [Move]
validMoves (State mx my p h@(x, y) w) = (,(x, y)) <$> vstarts
  where
    xs = zip [a | a <- [x -1, x + 1], a <= mx, a >= 0] $repeat y
    ys = zip (repeat x) [a | a <- [y -1, y + 1], a <= my, a >= 0]
    vstarts = filter (`S.notMember` w) $ xs <> ys

nextStates :: State -> Int -> [(Int, (State, Int, Move))]
nextStates s cc = am <$> validMoves s
  where
    am m = (cc + sCost s', (s', cc + 1, m)) where s' = moveState s m

initialState :: [((Int, Int), Node)] -> State
initialState l = State maxx maxy (maxx, 0) hole walls
  where
    hole = maybe (0, 0) fst $ find ((== 0) . used . snd) l
    maxx = maximum $ fst . fst <$> l
    maxy = maximum $ snd . fst <$> l
    walls = S.fromList $ filter isWall $ M.keys m
    m = M.fromList l
    cg = a + b where Node a b = m M.! (maxx, 0)
    isWall (x, y) = cu > cg where (Node cu ca) = m M.! (x, y)

-- >>> solve $ parse "root@ebhq-gridcenter# df -h\nFilesystem            Size  Used  Avail  Use%\n/dev/grid/node-x0-y0   10T    8T     2T   80%\n/dev/grid/node-x0-y1   11T    6T     5T   54%\n/dev/grid/node-x0-y2   32T   28T     4T   87%\n/dev/grid/node-x1-y0    9T    7T     2T   77%\n/dev/grid/node-x1-y1    8T    0T     8T    0%\n/dev/grid/node-x1-y2   11T    7T     4T   63%\n/dev/grid/node-x2-y0   10T    6T     4T   60%\n/dev/grid/node-x2-y1    9T    8T     1T   88%\n/dev/grid/node-x2-y2    9T    6T     3T   66%"
-- (7,7)

-- could probably get away with heuristics, like distance between initial and goal position avoiding walls plus 5* (maxx-1)
-- but this is fast enough ~10s interpreted, 800 millis compiled
solve :: [((Int, Int), Node)] -> (Int, Int)
solve l = (p1, p2)
  where
    p1 = length cns
    Just (_, p2, p2m) = aStar (P.singleton 0 initial) S.empty isGoal nextStates
    initial = (initialState l, 0, [])
    cns = [(a, b) | a <- l, b <- l, valid (snd a) (snd b)]
    valid a@(Node u _) b@(Node _ av) = u > 0 && a /= b && av >= u

main :: IO ()
main = readFile "input/day22.txt" >>= print . solve . parse

p2 (maxx, _) (hx, hy) = hy + maxx - hx + (maxx -1) * 5