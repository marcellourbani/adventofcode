#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Char (isDigit, toUpper)
import Data.List (findIndex, sort)

data Register = X | Y | Z | W deriving (Show, Eq, Read)

data Operand = L Int | R Register deriving (Show, Eq, Read)

data ALUOp
  = Inp Register
  | Add Register Operand
  | Mul Register Operand
  | Div Register Operand
  | Mod Register Operand
  | Eql Register Operand
  deriving (Show, Eq)

type Input = [ALUOp]

parse :: String -> Input
parse s = pi <$> lines (toUpper <$> s)
  where
    ro o = if isDigit (head o) || head o == '-' then L $read o else R $ read o
    pi i = case words i of
      ["INP", xx] -> Inp $ read xx
      ["ADD", xx, yy] -> Add (read xx) (ro yy)
      ["MUL", xx, yy] -> Mul (read xx) (ro yy)
      ["DIV", xx, yy] -> Div (read xx) (ro yy)
      ["MOD", xx, yy] -> Mod (read xx) (ro yy)
      ["EQL", xx, yy] -> Eql (read xx) (ro yy)
      _ -> error "Invalid operation"

chunk :: [ALUOp] -> [ALUOp] -> [[ALUOp]] -> [[ALUOp]]
chunk p c acc = case p of
  [] -> acc'
  op@(Inp _) : ops -> chunk ops [op] acc'
  op : ops -> chunk ops (c ++ [op]) acc
  where
    acc' = if null c then acc else acc ++ [c]

solve :: [ALUOp] -> (Int, Int)
solve i = (go p1vals multipliers [] [], go p2vals multipliers [] [])
  where
    chunks = chunk i [] []
    multipliers = zip [0 ..] $ relvalues <$> chunks

    p1vals mi si diff acc = (mi, min 9 (9 + diff)) : (si, min 9 (9 - diff)) : acc
    p2vals mi si diff acc = (mi, max 1 (1 + diff)) : (si, max 1 (1 - diff)) : acc

    go f muls stack acc = case (muls, stack) of
      (m@(_, (1, _)) : ms, _) -> go f ms (m : stack) acc
      (m@(mi, (_, mv)) : ms, s@(si, (_, sv)) : ss) -> go f ms ss acc'
        where
          diff = mv + sv
          acc' = f mi si diff acc
      _ -> read $ concat $ show . snd <$> sort acc

    relvalues v = case v !! 4 of
      Div _ (L i) -> (i, p)
        where
          p = case v !! (if i == 1 then 15 else 5) of
            Add _ (L v) -> v
            _ -> error "unexpected"
      _ -> error "unexpected"

main :: IO ()
main = readFile "input/day24.txt" >>= print . solve . parse