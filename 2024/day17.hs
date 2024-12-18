#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

module Main where

import Data.Bits (Bits (shiftL), shiftL, shiftR, xor)
import Data.List.Split (splitOn)
import Debug.Trace (trace)

data OpCode = ADV | BXL | BST | JNZ | BXC | OUT | BDV | CDV deriving (Show, Eq, Enum)

data CPU = CPU {cpA :: Int, cpB :: Int, cpC :: Int, cpIp :: Int, cpMem :: [(OpCode, Int)]} deriving (Show)

parse :: String -> CPU
parse s = CPU a b c 0 mem
  where
    [ra, rb, rc, _, p] = lines s
    ops m = case m of
      c : o : r -> (toEnum c, o) : ops r
      _ -> []
    mem = ops $ read <$> splitOn "," (drop 9 p)
    [a, b, c] = read . drop 12 <$> [ra, rb, rc]

combo :: CPU -> Int -> Int
combo cpu o
  | o >= 0 && o <= 3 = o
  | o == 4 = cpA cpu
  | o == 5 = cpB cpu
  | o == 6 = cpC cpu
  | o == 7 = undefined

execOp :: CPU -> Maybe (CPU, [Int])
execOp cpu@(CPU a b c ip mem)
  | div ip 2 >= length mem = Nothing
  | otherwise = Just $ case opcode of
      ADV -> (cpu' {cpA = adv}, [])
      BDV -> (cpu' {cpB = adv}, [])
      CDV -> (cpu' {cpC = adv}, [])
      BXC -> (cpu' {cpB = xor b c}, [])
      BXL -> (cpu' {cpB = xor b operand}, [])
      BST -> (cpu' {cpB = mod8 $ combo cpu operand}, [])
      JNZ
        | a == 0 -> (cpu', [])
        | otherwise -> (cpu {cpIp = operand}, [])
      OUT -> (cpu', [mod8 $ combo cpu operand])
  where
    mod8 = (`mod` 8)
    (opcode, operand) = mem !! div ip 2
    cpu' = cpu {cpIp = ip + 2}
    adv = shiftR a $ combo cpu operand

part1 cpu = case execOp cpu of
  Nothing -> []
  Just (cpu', [o]) -> show o <> part1 cpu'
  Just (cpu', _) -> part1 cpu'

-- part2 :: Input -> Int
part2 l = 0

-- >>> solve $ parse "Register A: 729\nRegister B: 0\nRegister C: 0\n\nProgram: 0,1,5,4,3,0"
-- ("4635635210",0)

solve l = (p1, p2)
  where
    p1 = part1 l
    p2 = part2 l

main :: IO ()
main = readFile "input/day17.txt" >>= print . solve . parse
