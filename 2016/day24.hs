#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE TupleSections #-}

module Main where

import Data.Bifunctor.Swap
import Data.List (sortOn)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.PQueue.Prio.Min as P
import qualified Data.Set as S
import Debug.Trace (trace)

type Input = (M.Map Char (Int, Int), S.Set (Int, Int))

type Move = (Int, Int)

data State = State {snumbers :: M.Map Char (Int, Int), spos :: (Int, Int), grid :: S.Set (Int, Int)} deriving (Show, Eq, Ord)

parse :: String -> Input
parse s = (toVisit, S.fromList notWalls)
  where
    rawMap = pl =<< zip [0 ..] (lines s)
    notWalls = fst <$> filter ((/= '#') . snd) rawMap
    toVisit = M.fromList $ swap <$> filter ((`elem` "1234567890") . snd) rawMap
    pl (y, r) = zip (zip [0 ..] (repeat y)) r

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

nextMoves :: State -> [Move]
nextMoves (State _ (x, y) v) = [p | p <- ((,y) <$> [x - 1, x + 1]) <> ((x,) <$> [y - 1, y + 1]), S.member p v]

isGoal :: State -> Bool
isGoal (State n _ _) = M.null n

costEstimate :: State -> Int
costEstimate (State n l _) = go l $ filter (/= l) $ snd <$> M.toList n
  where
    md (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
    go p l = case sortOn (md p) l of
      [] -> 0
      x : xs -> md p x + go x xs

applyMove :: State -> Move -> State
applyMove (State sn _ gr) m = State (M.filter (/= m) sn) m gr

nextStates :: State -> Int -> [(Int, (State, Int, Move))]
nextStates s c = nm <$> moves
  where
    moves = nextMoves s
    nm m = (c + costEstimate s', (s', c + 1, m)) where s' = applyMove s m

-- >>> solve $ parse "###########\n#0.1.....2#\n#.#######.#\n#4.......3#\n###########"
-- 14

solve (ns, m) = (p1)
  where
    initialState = State ns (ns M.! '0') m
    Just (_, p1, _) = aStar (P.singleton 0 (initialState, 0, [])) S.empty isGoal nextStates

main :: IO ()
main = readFile "input/day24.txt" >>= print . solve . parse
