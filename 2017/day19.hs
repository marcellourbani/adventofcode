#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import qualified Data.Map.Strict as M
import Data.Maybe (isJust, isNothing)

data Direction = U | D | L | R deriving (Show, Eq)

data State = State {position :: (Int, Int), direction :: Direction, collected :: String, done :: Bool, count :: Int} deriving (Show)

type Network = M.Map (Int, Int) Char

parse :: String -> M.Map (Int, Int) Char
parse s = M.fromList [((x, y), c) | (y, l) <- zip [0 ..] $ lines s, (x, c) <- zip [0 ..] l, c /= ' ']

nextState :: Network -> State -> State
nextState net state@(State p d c don co) = if don then state else State pos' dir' c' done' co'
  where
    done' = isNothing cell || cell == Just ' '
    co' = co + 1
    pos' = nextPos p d
    c' = case cell of
      (Just ch) | ch `elem` ['A' .. 'Z'] -> c <> [ch]
      _ -> c
    dir' = case (cell, d) of
      (Just '+', U) -> tohor pos'
      (Just '+', D) -> tohor pos'
      (Just '+', L) -> tover pos'
      (Just '+', R) -> tover pos'
      _ -> d
    cell = net M.!? pos'
    tohor (x, y) = if isJust $ net M.!? (x + 1, y) then R else L
    tover (x, y) = if isJust $net M.!? (x, y + 1) then D else U
    nextPos (x, y) di = case di of
      U -> (x, y -1)
      D -> (x, y + 1)
      L -> (x -1, y)
      R -> (x + 1, y)

-- >>> solve $ parse "     |          \n     |  +--+    \n     A  |  C    \n F---|----E|--+ \n     |  |  |  D \n     +B-+  +--+ "
-- ("ABCDEF",38)

solve :: M.Map (Int, Int) Char -> (String, Int)
solve l = (p1, p2)
  where
    p1 = collected final
    p2 = count final
    final = head $ filter done $ iterate (nextState l) initial
    initial = State start D "" False 0
    start = head $ M.keys $ M.filterWithKey (\(_, y) _ -> y == 0) l

main :: IO ()
main = readFile "input/day19.txt" >>= print . solve . parse
