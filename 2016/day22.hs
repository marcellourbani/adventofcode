#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE TupleSections #-}

module Main where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.PQueue.Prio.Min as P
import qualified Data.Set as S

data Node = Node {used :: Int, avail :: Int} deriving (Show, Eq, Ord)

data State = State {position :: (Int, Int), nodes :: M.Map (Int, Int) Node} deriving (Show, Eq, Ord)

type Move = ((Int, Int), (Int, Int))

parse :: String -> [((Int, Int), Node)]
parse s = pl <$> drop 2 (lines s)
  where
    pl s = ((x, y), Node usd avl)
      where
        (r : re) = words . drop 16 . filter (`notElem` "T%") $s
        [x, y] = read <$> splitOn "-y" r
        [_, usd, avl, _] = read <$> re

aStar :: Ord s => Show m => P.MinPQueue Int (s, Int, [m]) -> S.Set s -> (s -> Bool) -> (s -> Int -> [(Int, (s, Int, m))]) -> Maybe (s, Int, [m])
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
isGoal (State (x, y) _) = x == 0 && y == 0

findHole :: State -> (Int, Int)
findHole (State _ g) = fst $ head $ M.toList $ M.filter ((== 0) . used) g

sCost :: State -> Int
sCost s@(State (x, y) g) = 10 * (x + y) + holedist
  where
    (xh, yh) = findHole s
    holedist = abs (xh - x) + abs (yh - y)

moveState :: State -> Move -> State
moveState (State p nodes) (p1, p2) = State p' nodes'
  where
    (Node us1 av1) = nodes M.! p1
    (Node us2 av2) = nodes M.! p2
    nodes' = M.insert p1 (Node 0 (us1 + av1)) $ M.insert p2 (Node (us1 + us2) (av2 - us1)) nodes
    p' = if p == p1 then p2 else p

validMoves :: State -> [Move]
validMoves (State _ nodes) = nk >>= nn
  where
    nk = M.keys nodes
    nn (x, y) = ((x, y),) <$> filter (vm (x, y)) [(x + 1, y), (x -1, y), (x, y + 1), (x, y -1)]
    vm p (x', y') = case M.lookup (x', y') nodes of
      Nothing -> False
      Just (Node _ av) -> un1 > 0 && av >= un1 where un1 = used $ nodes M.! p

nextStates :: State -> Int -> [(Int, (State, Int, Move))]
nextStates s cc = am <$> validMoves s
  where
    am m = (cc + sCost s', (s', cc + 1, m)) where s' = moveState s m

-- >>> solve $ parse "root@ebhq-gridcenter# df -h\nFilesystem            Size  Used  Avail  Use%\n/dev/grid/node-x0-y0   10T    8T     2T   80%\n/dev/grid/node-x0-y1   11T    6T     5T   54%\n/dev/grid/node-x0-y2   32T   28T     4T   87%\n/dev/grid/node-x1-y0    9T    7T     2T   77%\n/dev/grid/node-x1-y1    8T    0T     8T    0%\n/dev/grid/node-x1-y2   11T    7T     4T   63%\n/dev/grid/node-x2-y0   10T    6T     4T   60%\n/dev/grid/node-x2-y1    9T    8T     1T   88%\n/dev/grid/node-x2-y2    9T    6T     3T   66%"
-- (7,7)

solve :: [((Int, Int), Node)] -> (Int, Int)
solve l = (p1, p2)
  where
    p1 = length cns
    Just (_, p2, _) = aStar (P.singleton 0 initialState) S.empty isGoal nextStates
    -- p2 = aStar (P.singleton 0 initialState) S.empty isGoal nextStates
    maxx = maximum $ fst . fst <$> l
    initialState = (State (maxx, 0) $ M.fromList l, 0, [])
    cns = [(a, b) | a <- l, b <- l, valid (snd a) (snd b)]
    valid a@(Node u _) b@(Node _ av) = u > 0 && a /= b && av >= u

main :: IO ()
main = readFile "input/day22.txt" >>= print . solve . parse
