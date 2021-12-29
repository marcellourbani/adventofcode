#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Char (isDigit, toUpper)
import Data.List (findIndex)

data Register = X | Y | Z | W deriving (Show, Eq, Read)

data Operand = L Int | R Register deriving (Show, Eq, Read)

data ALUOp
  = Inp Register
  | Add Operand Operand
  | Mul Operand Operand
  | Div Operand Operand
  | Mod Operand Operand
  | Eql Operand Operand
  deriving (Show, Eq)

data Value
  = Lit Int
  | Var Int
  | VAdd Value Value
  | VMul Value Value
  | VDiv Value Value
  | VMod Value Value
  | VEql Value Value
  deriving (Eq)

instance Show Value where
  show v = case v of
    Lit i -> show i
    Var i -> "V" <> show i
    VAdd v1 v2 -> "(" <> show v1 <> " + " <> show v2 <> ")"
    VMul v1 v2 -> "(" <> show v1 <> " * " <> show v2 <> ")"
    VDiv v1 v2 -> "(" <> show v1 <> " / " <> show v2 <> ")"
    VMod v1 v2 -> "(" <> show v1 <> " % " <> show v2 <> ")"
    VEql v1 v2 -> "(" <> show v1 <> " == " <> show v2 <> ")"

data ALU = ALU {x :: Int, y :: Int, z :: Int, w :: Int} deriving (Show)

data SALU = SALU {sx :: Value, sy :: Value, sz :: Value, sw :: Value, lastvar :: Int} deriving (Show)

type Input = [ALUOp]

parse :: String -> Input
parse s = pi <$> lines (toUpper <$> s)
  where
    ro o = if isDigit (head o) || head o == '-' then L $read o else R $ read o
    pi i = case words i of
      ["INP", xx] -> Inp $ read xx
      ["ADD", xx, yy] -> Add (ro xx) (ro yy)
      ["MUL", xx, yy] -> Mul (ro xx) (ro yy)
      ["DIV", xx, yy] -> Div (ro xx) (ro yy)
      ["MOD", xx, yy] -> Mod (ro xx) (ro yy)
      ["EQL", xx, yy] -> Eql (ro xx) (ro yy)
      _ -> error "Invalid operation"

readOp :: ALU -> Operand -> Int
readOp a o = case o of
  L v -> v
  R X -> x a
  R Y -> y a
  R Z -> z a
  R W -> w a

storeOp :: ALU -> Register -> Int -> ALU
storeOp a r v = case r of
  X -> a {x = v}
  Y -> a {y = v}
  Z -> a {z = v}
  W -> a {w = v}

execute :: ALU -> [ALUOp] -> [Int]
execute alu prog = go alu chunks
  where
    go a chs = case chs of
      [] -> []
      c : cs -> d : go a' cs
        where
          (d, a') = digit a [9, 8 .. 1] c

    digit a cand prog = case cand of
      [] -> error "no valid digit found"
      d : ds -> case executeChunk a d prog of
        Just a' | z a' == 0 -> (d, a')
        _ -> digit a ds prog

    chunks = chunk prog [] []
    chunk p c acc = case p of
      [] -> acc'
      op@(Inp _) : ops -> chunk ops [op] acc'
      op : ops -> chunk ops (c ++ [op]) acc
      where
        acc' = if null c then acc else acc ++ [c]

spv x = case x of
  VAdd v1 v2 -> Just (VAdd, v1, v2)
  VMul v1 v2 -> Just (VMul, v1, v2)
  -- VDiv v1 v2 -> Just (VDiv, v1, v2)
  _ -> Nothing

vAdd :: Value -> Value -> Value
vAdd a b = case (a, b) of
  (_, Lit 0) -> a
  (Lit 0, _) -> b
  (Lit x, Lit y) -> Lit $ x + y
  _ -> VAdd a b

vMul :: Value -> Value -> Value
vMul a b = case (a, b) of
  (Lit 0, _) -> Lit 0
  (_, Lit 0) -> Lit 0
  (Lit 1, _) -> b
  (_, Lit 1) -> a
  (Lit x, Lit y) -> Lit $ x * y
  (VAdd x y, _) -> VAdd (vMul x b) (vMul y b)
  (VMul x (Lit y), Lit z) -> VMul x (Lit $y * z)
  (_, VAdd x y) -> VAdd (vMul a x) (vMul a y)
  _ -> VMul a b

vDiv :: Value -> Value -> Value
vDiv a b = case (a, b) of
  (_, Lit 1) -> a
  (Lit x, Lit y) -> Lit $ div x y
  _ -> VDiv a b

vMod :: Value -> Value -> Value
vMod a b = case (a, b) of
  (_, Lit 1) -> Lit 0
  (Lit x, Lit y) -> Lit $ mod x y
  _ -> VMod a b

vEql :: Value -> Value -> Value
vEql a b = case (a, b, a == b) of
  (_, _, True) -> Lit 0
  (Lit x, Lit y, _) -> Lit 1
  _ -> VEql a b

executeSym :: SALU -> [ALUOp] -> SALU
executeSym = go
  where
    go cur prog = case prog of
      [] -> cur
      (Inp op) : rest -> go ((st op (Var v)) {lastvar = v}) rest
        where
          v = 1 + lastvar cur
      (Add a@(R ar) b) : rest -> go (st ar (ro a `vAdd` ro b)) rest
      (Mul a@(R ar) b) : rest -> go (st ar (ro a `vMul` ro b)) rest
      (Div a@(R ar) b) : rest -> go (st ar (ro a `vDiv` ro b)) rest
      (Mod a@(R ar) b) : rest -> go (st ar (ro a `vMod` ro b)) rest
      (Eql a@(R ar) b) : rest -> go (st ar (Lit $ if ro a == ro b then 1 else 0)) rest
      _ -> cur
      where
        ro o = case o of
          L v -> Lit v
          R X -> sx cur
          R Y -> sy cur
          R Z -> sz cur
          R W -> sw cur
        st r v = case r of
          X -> cur {sx = v}
          Y -> cur {sy = v}
          Z -> cur {sz = v}
          W -> cur {sw = v}

executeChunk :: ALU -> Int -> [ALUOp] -> Maybe ALU
executeChunk a iv prog = go a prog
  where
    go cur prog = case prog of
      [] -> Just cur
      (Inp op) : rest -> go (storeOp cur op iv) rest
      (Add a@(R ar) b) : rest -> go (storeOp cur ar (readOp cur a + readOp cur b)) rest
      (Mul a@(R ar) b) : rest -> go (storeOp cur ar (readOp cur a * readOp cur b)) rest
      (Div a@(R ar) b) : rest -> go (storeOp cur ar (readOp cur a `div` readOp cur b)) rest
      (Mod a@(R ar) b) : rest -> go (storeOp cur ar (readOp cur a `mod` readOp cur b)) rest
      (Eql a@(R ar) b) : rest -> go (storeOp cur ar (if readOp cur a == readOp cur b then 1 else 0)) rest
      _ -> Nothing

-- >>> initial = SALU (Lit 0) (Lit 0) (Lit 0) (Lit 0) 0
-- >>> pr = parse "inp w\nmul x 0\nadd x z\nmod x 26\ndiv z 1\nadd x 10\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 12\nmul y x\nadd z y"
-- >>> pr2 = parse "inp w\nmul x 0\nadd x z\nmod x 26" -- \ndiv z 1\nadd x 10\neql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\nmul z y\nmul y 0\nadd y w\nadd y 12\nmul y x\nadd z y"
-- >>> executeSym initial pr
-- SALU {sx = 1, sy = V1 + 12, sz = V1 + 12, sw = V1, lastvar = 1}

-- SALU {sx = Lit 1, sy = VMul (VAdd (Var 1) (Lit 12)) (Lit 1), sz = VMul (VAdd (Var 1) (Lit 12)) (Lit 1), sw = Var 1, lastvar = 1}

-- >>> solve  $parse "inp w\nadd z w\nmod z 2\ndiv w 2\nadd y w\nmod y 2\ndiv w 2\nadd x w\nmod x 2\ndiv w 2\nmod w 2"
-- V1 % 2

solve i = sz $ executeSym initial i where initial = SALU (Lit 0) (Lit 0) (Lit 0) (Lit 0) 0

main :: IO ()
main = readFile "input/day24.txt" >>= print . solve . parse

-- ((((((((((((((((((((((((((((((((((V1 + 12) * 26) + (V2 + 10)) * 26) + (V3 + 8)) * 26) + (V4 + 4)) / 26) * 26) + (V5 + 3)) * 26) + (V6 + 10)) * 26) + (V7 + 6)) / 26) * 26) + (V8 + 13)) / 26) * 26) + (V9 + 8)) / 26) * 26) + (V10 + 1)) / 26) * 26) + (V11 + 7)) * 26) + (V12 + 6)) / 26) * 26) + (V13 + 9)) / 26) * 26) + (V14 + 9))