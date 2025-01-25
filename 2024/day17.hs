#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

module Main where

import Data.Bits (Bits (shiftL), shiftL, shiftR, xor)
import Data.List (intercalate)
import Data.List.Split (splitOn)

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
  | o <= 3 = o
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

process :: CPU -> [Int]
process cpu = case execOp cpu of
  Nothing -> []
  Just (cpu', [o]) -> o : process cpu'
  Just (cpu', _) -> process cpu'

part2 :: CPU -> Int
part2 cpu = go 0 $ mle - 1
  where
    toI (a, b) = [fromEnum a, b]
    p8 n = floor $ 8 ^^ n
    mem = cpMem cpu >>= toI
    mle = length mem
    go n dr
      | curs == mem = n
      | drop dr mem == curs = go (n * 8) (dr - 1)
      | dr < 0 = undefined
      | otherwise = go (n + 1) dr
      where
        -- \| otherwise = go n + inc
        curs = process cpu {cpA = n}
        mat = length $ takeWhile (uncurry (==)) $ zip mem curs
        inc = p8 $ max 0 (mat - 3)

-- >>> process $ parse "Register A: 729\nRegister B: 0\nRegister C: 0\n\nProgram: 0,1,5,4,3,0"
-- >>> solve $ parse "Register A: 2024\nRegister B: 0\nRegister C: 0\n\nProgram: 0,3,5,4,3,0"
-- [4,6,3,5,6,3,5,2,1,0]
-- ("5,7,3,0",(117440,[0,3,5,4,3,0]))

solve l = (p1, p2)
  where
    p1 = intercalate "," $ show <$> process l
    p2 = part2 l

main :: IO ()
main = readFile "input/day17.txt" >>= print . solve . parse
