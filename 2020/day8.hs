#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List

sample :: [Char]
sample = "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6"

data Opcode = Nop | Acc | Jmp deriving (Show, Eq)

data Cell = Cell {op :: Opcode, parm :: Int, visited :: Bool} deriving (Show)

data State = State {pc :: Int, accumulator :: Int, cells :: [Cell]} deriving (Show)

execOp :: State -> State
execOp (State pc acc cells) = State newpc newacc newcells
  where
    newcell = (cells !! pc) {visited = True}
    newcells = take pc cells ++ newcell : drop (pc + 1) cells
    newpc = case newcell of
      Cell Jmp par _ -> pc + par
      _ -> pc + 1
    newacc = case newcell of
      Cell Acc par _ -> acc + par
      _ -> acc

parse :: String -> [Cell]
parse = fmap (decOp . words) . lines
  where
    decOp :: [String] -> Cell
    decOp (op : (s : p) : _) = Cell (decCode op) par False
      where
        par = case s of
          '+' -> read p
          _ -> read $ s : p
    decCode s = case s of
      "acc" -> Acc
      "jmp" -> Jmp
      "nop" -> Nop

-- >>> solve sample
-- (5,8)

solve :: String -> (Int, Int)
solve s = (accumulator final, maybe 0 accumulator patched)
  where
    initial = State 0 0 $ parse s
    final = go initial
    go (State pc acc cells)
      | pc >= length cells || visited (cells !! pc) = State pc acc cells
      | otherwise = go $ execOp (State pc acc cells)
    jumps = [(n, j {op = Nop}) | (n, j) <- zip [0 ..] $ cells initial, op j == Jmp]
    completed s = pc s >= length (cells s)
    patchState state (i, cell) = state {cells = take i cs ++ cell : drop (i + 1) cs} where cs = cells state
    patched = find completed $ fmap (go . patchState initial) jumps

main :: IO ()
main = readFile "input/day8.txt" >>= print . solve
