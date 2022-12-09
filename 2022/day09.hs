#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (Foldable (foldl'))
import qualified Data.Set as S
import Debug.Trace (trace)

data Direction = L | U | R | D deriving (Read, Show)

type Move = (Direction, Int)

data State = State {sHead :: (Int, Int), sTail :: (Int, Int), sVis :: S.Set (Int, Int)} deriving (Show)

parse :: String -> [Move]
parse s = readline . words <$> lines s
  where
    readline [a, b] = (read a, read b)
    readline _ = undefined

evolve :: State -> Move -> State
evolve s@(State h@(hx, hy) (tx, ty) vis) m@(dir, ml) = case (ml, touching) of
  (0, True) -> s
  (0, False) -> evolve (s {sTail = t', sVis = vis'}) m
  (_, _) -> evolve s' (dir, ml -1)
  where
    moveTail hc tc = if abs d > 1 then tc + div d (abs d) else tc where d = hc - tc
    t' = case (tx' == tx, ty' == ty) of
      (True, False) | tx' /= hx -> (hx, ty')
      (False, True) | ty' /= hy -> (tx', hy)
      (_, _) -> (tx', ty')
      where
        (tx', ty') = (moveTail hx tx, moveTail hy ty)
    vis' = S.insert t' vis
    touching = abs (hx - tx) <= 1 && abs (hy - ty) <= 1
    h'@(hx', hy') = case m of
      (_, 0) -> h
      (L, _) -> (hx -1, hy)
      (R, _) -> (hx + 1, hy)
      (U, _) -> (hx, hy -1)
      (D, _) -> (hx, hy + 1)
    s' = State h' t' vis'

-- >>> solve $ parse "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"
-- 13

solve :: [Move] -> Int
solve l = p1
  where
    initial = State (0, 0) (0, 0) $ S.singleton (0, 0)
    p1 = S.size . sVis $ foldl' evolve initial l
    ll = parse "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"
    p1b = foldl' evolve initial $ take 2 ll

main :: IO ()
main = readFile "input/day09.txt" >>= print . solve . parse
