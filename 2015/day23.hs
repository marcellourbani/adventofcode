#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Char (toUpper)

data CPU = CPU {_a :: Int, _b :: Int, _pc :: Int} deriving (Show, Eq)

data Register = A | B deriving (Show, Eq, Read)

data OpCode
  = HLF Register
  | TPL Register
  | INC Register
  | JMP Int
  | JIE Register Int
  | JIO Register Int
  deriving (Show, Eq, Read)

parse :: [Char] -> [OpCode]
parse i = pl <$> lines (filter (`notElem` "+,") $ toUpper <$> i)
  where
    pl l = case words l of
      ["JIE", r, n] -> JIE (read r) (read n)
      ["JIO", r, n] -> JIO (read r) (read n)
      _ -> read l

runOpCode :: CPU -> OpCode -> CPU
runOpCode (CPU a b pc) o = case o of
  HLF r -> apply (`div` 2) r
  TPL r -> apply (* 3) r
  INC r -> apply (+ 1) r
  JMP n -> CPU a b (pc + n)
  JIE r n -> if even (val r) then CPU a b (pc + n) else CPU a b $pc + 1
  JIO r n -> if val r == 1 then CPU a b (pc + n) else CPU a b $pc + 1
  where
    val r = case r of
      A -> a
      B -> b
    apply f r = case r of
      A -> CPU (f a) b (pc + 1)
      B -> CPU a (f b) (pc + 1)

runProg :: CPU -> [OpCode] -> CPU
runProg c@(CPU _ _ pc) prog
  | pc < 0 || pc >= length prog = c
  | otherwise = runProg (runOpCode c (prog !! pc)) prog

-- >>> solve $ parse"inc a\njio a, +2\ntpl a\ninc a"
-- 0

solve :: [OpCode] -> (Int, Int)
solve a = (p1, p2)
  where
    p1 = _b $ runProg (CPU 0 0 0) a
    p2 = _b $ runProg (CPU 1 0 0) a

main :: IO ()
main = readFile "input/day23.txt" >>= print . solve . parse
