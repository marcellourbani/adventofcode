#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List (partition)
import qualified Data.Map.Strict as M

data Cucumber = E | S deriving (Show, Eq)

type CucOnBoard = ((Int, Int), Cucumber)

data Board = Board {maxx :: Int, maxy :: Int, grid :: M.Map (Int, Int) Cucumber} deriving (Eq)

type Input = Board

instance Show Board where
  show (Board mx my gr) = show mx <> " * " <> show my <> "\n" <> mg
    where
      ll y = [c (x, y) | x <- [0 .. mx -1]]
      mg = unlines $ ll <$> [0 .. my - 1]
      c (x, y) = case M.lookup (x, y) gr of
        Just E -> '>'
        Just S -> 'v'
        _ -> '.'

parse :: String -> Input
parse s = Board mx my gr
  where
    ls = lines s
    my = length ls
    mx = length $ head ls
    pl (y, l) = [((x, y), cu) | (x, c) <- zip [0 ..] l, c /= '.', let cu = if c == '>' then E else S]
    gr = M.fromList $ zip [0 ..] ls >>= pl

nextPos :: Board -> CucOnBoard -> CucOnBoard
nextPos (Board mx my gr) c@((x, y), d) = case M.lookup np gr of
  Nothing -> (np, d)
  _ -> c
  where
    np = case d of
      E -> (mod (x + 1) mx, y)
      S -> (x, mod (y + 1) my)

nextBoard :: Board -> Board
nextBoard b = b {grid = M.fromList $ movedeast ++ moveddown}
  where
    (es, ss) = partition ((== E) . snd) $ M.toList (grid b)
    movedeast = nextPos <$> [b] <*> es
    interm = M.fromList $ss ++ movedeast
    moveddown = nextPos <$> [b {grid = interm}] <*> ss

-- >>>   solve  $parse "v...>>.vv>\n.vv>>.vv..\n>>.>v>...v\n>>v>>.>.v.\nv>v.vv.v..\n>.>>..v...\n.vv..>.>v.\nv.v..>>v.v\n....v..v.>"
-- 58

solve :: Input -> Int
solve i = go i 0
  where
    go b acc = if nb == b then acc + 1 else go nb $ acc + 1
      where
        nb = nextBoard b

main :: IO ()
main = readFile "input/day25.txt" >>= print . solve . parse
