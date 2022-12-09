#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (Foldable (foldl'))
import qualified Data.Set as S

data Direction = L | U | R | D deriving (Read, Show)

type Move = (Direction, Int)

data State = State {sHead :: (Int, Int), sTail :: [(Int, Int)], sVis :: S.Set (Int, Int)} deriving (Show)

parse :: String -> [Move]
parse s = readline . words <$> lines s
  where
    readline [a, b] = (read a, read b)
    readline _ = undefined

follow :: (Int, Int) -> (Int, Int) -> (Int, Int)
follow h@(hx, hy) (tx, ty) = case (tx' == tx, ty' == ty) of
  (True, False) | tx' /= hx -> (hx, ty')
  (False, True) | ty' /= hy -> (tx', hy)
  (_, _) -> (tx', ty')
  where
    (tx', ty') = (moveTail hx tx, moveTail hy ty)
    moveTail hc tc = if abs d > 1 then tc + div d (abs d) else tc where d = hc - tc

touching :: (Int, Int) -> (Int, Int) -> Bool
touching h@(hx, hy) (tx, ty) = abs (hx - tx) <= 1 && abs (hy - ty) <= 1

evolve :: State -> Move -> State
evolve s@(State h@(hx, hy) t vis) m@(dir, ml) = case (ml, joined $ h : t) of
  (0, True) -> s'
  (0, False) -> evolve (s {sTail = t', sVis = vis'}) m
  (_, _) -> evolve s' (dir, ml -1)
  where
    moveTail hc tc = if abs d > 1 then tc + div d (abs d) else tc where d = hc - tc
    ft he ta = case ta of
      [] -> []
      x : xs -> follow he x : ft x xs
    joined l = case l of
      a : b : xs -> touching a b && joined (b : xs)
      _ -> True
    t' = ft h t
    vis' = S.insert (last t) vis
    h'@(hx', hy') = case m of
      (_, 0) -> h
      (L, _) -> (hx -1, hy)
      (R, _) -> (hx + 1, hy)
      (U, _) -> (hx, hy -1)
      (D, _) -> (hx, hy + 1)
    s' = State h' t' vis'

-- >>> solve $ parse "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"
-- >>> solve $ parse "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20"
-- (13,1)
-- (88,36)

solve :: [Move] -> (Int, Int)
solve l = (p1, p2)
  where
    initial = State (0, 0) [(0, 0)] $ S.singleton (0, 0)
    initial2 = initial {sTail = replicate 9 (0, 0)}
    p1 = S.size . sVis $ foldl' evolve initial l
    p2 = S.size . sVis $ foldl' evolve initial2 l

main :: IO ()
main = readFile "input/day09.txt" >>= print . solve . parse
