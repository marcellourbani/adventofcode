#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import Algorithm.Search (aStar)
import Data.List (elemIndex)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Vector as V

data Direction = U | D | L | R deriving (Show, Eq)

type Blizzards = M.Map (Int, Int) [Direction]

newtype FreeCoords = FreeCoords {unFreeCoords :: S.Set (Int, Int)}

data Valley = Valley {vMaxx :: Int, vMaxy :: Int, vStart :: Int, vEnd :: Int, vBlizs :: Blizzards} deriving (Show)

data Status = Status (Int, Int) Int deriving (Show, Eq, Ord)

instance Show FreeCoords where
  show (FreeCoords s) = unlines $ line <$> [1 .. my]
    where
      mx = maximum $ fst <$> S.toList s
      my = maximum $ snd <$> S.toList s
      ch c = if S.member c s then ' ' else '.'
      line y = ch . (,y) <$> [1 .. mx]

parse :: String -> Valley
parse s = Valley maxx maxy enter exit $ M.fromList cs
  where
    fc c = case c of
      '^' -> U
      'v' -> D
      '<' -> L
      '>' -> R
      _ -> undefined
    enter = fromMaybe 0 $ elemIndex '.' s
    exit = fromMaybe 0 $ elemIndex '.' $ last $ lines s
    maxx = length (head $ lines s) - 2
    maxy = length (lines s) - 2
    cs = [((x, y), [fc c]) | (y, l) <- zip [1 .. maxy] $ tail $ lines s, (x, c) <- zip [1 .. maxx] $ tail l, c /= '.']

-- module, but with offset 1
mod1 :: Int -> Int -> Int
mod1 v l = 1 + mod (v - 1) l

moveBlizzards :: Int -> Int -> Blizzards -> Blizzards
moveBlizzards mx my bs = bs'
  where
    moveB (x, y) d = case d of
      U -> (x, mod1 (y - 1) my)
      D -> (x, mod1 (y + 1) my)
      L -> (mod1 (x - 1) mx, y)
      R -> (mod1 (x + 1) mx, y)
    moveBs (k, d) = M.unionsWith (<>) [M.singleton (moveB k x) [x] | x <- d]
    bs' = M.unionsWith (<>) $ moveBs <$> M.toList bs

getFreeFromBlizzards :: Valley -> (Int -> FreeCoords)
getFreeFromBlizzards (Valley mx my sx ex bs) = freeAtT
  where
    period = lcm mx my
    startp = (sx, 0)
    goal = (ex, my + 1)
    free b = FreeCoords $ S.fromList $ goal : startp : [(x, y) | x <- [1 .. mx], y <- [1 .. my], M.notMember (x, y) b]
    frees = V.fromList $ free <$> take period (iterate (moveBlizzards mx my) bs)
    freeAtT t = frees V.! mod t period

nextStates :: Int -> (Int -> FreeCoords) -> Status -> [Status]
nextStates maxt f (Status (x, y) t) = Status <$> nextpos <*> [mod1 (t + 1) maxt]
  where
    nextposraw = (x, y) : map (,y) [x - 1, x + 1] <> map (x,) [y - 1, y + 1]
    frees = f $ t + 1
    nextpos = filter (`S.member` unFreeCoords frees) nextposraw

-- >>> solve $ parse "#.######\n#>>.<^<#\n#.<..<<#\n#>v.><>#\n#<^v^^>#\n######.#"
-- 18
solve v@(Valley mx my ix ex bs) = p1
  where
    nexts = nextStates (lcm mx my) $ getFreeFromBlizzards v
    trcost = const . const 1
    cost (Status (x, y) t) = my + 1 - y + abs ex - x
    isGoal (Status c _) = c == (ex, my + 1)
    initial = Status (ix, 0) 0
    p1s = aStar nexts trcost cost isGoal initial
    p1 = maybe 0 fst p1s

main :: IO ()
main = readFile "input/day24.txt" >>= print . solve . parse
