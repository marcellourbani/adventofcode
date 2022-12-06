#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Either (fromRight)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as M
import Data.Void (Void)
import Text.Megaparsec (Parsec, empty, many, runParser, some, try, (<|>))
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char as LC
import qualified Text.Megaparsec.Char.Lexer as L

data Operand = Register Char | Literal Int deriving (Show)

data Instruction
  = Snd Operand
  | Set Char Operand
  | Add Char Operand
  | Mul Char Operand
  | Mod Char Operand
  | Rcv Char
  | Jgz Operand Operand
  deriving (Show)

data State = State {registers :: M.Map Char Int, frequency :: Int, recovered :: Int, pc :: Int} deriving (Show)

data CpuState = Running | Waiting | Done deriving (Show, Eq)

data Cpu = Cpu {cid :: Int, cregisters :: M.Map Char Int, state :: CpuState, pco :: Int, inbox :: [Int], sent :: Int} deriving (Show)

type Parser = Parsec Void String

sc :: Parser () -- consume spaces
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

operand :: Parser Operand
operand = lexeme $ (L.signed sc L.decimal <&> Literal) <|> Register <$> LC.lowerChar

instruction :: Parser Instruction
instruction =
  try (symbol "snd" *> (Snd <$> op))
    <|> (symbol "set" *> (Set <$> c <*> op))
    <|> (symbol "add" *> (Add <$> c <*> op))
    <|> (symbol "mul" *> (Mul <$> c <*> op))
    <|> (symbol "mod" *> (Mod <$> c <*> op))
    <|> (symbol "rcv" *> (Rcv <$> c))
    <|> (symbol "jgz" *> (Jgz <$> op <*> op))
  where
    op = lexeme operand
    c = lexeme LC.lowerChar

parse :: String -> [Instruction]
parse s = fromRight [] $ runParser (many instruction) "" s

runProgram :: M.Map Int Instruction -> (State -> Bool) -> State
runProgram prog excond = go initial
  where
    initial = State M.empty 0 0 0
    go state@(State reg freq recf c)
      | excond state = state
      | otherwise = case prog M.!? c of
        Nothing -> state
        Just (Set r v) -> go $ state' {registers = M.insert r (eval reg v) reg}
        Just (Add r v) -> go $ state' {registers = M.adjust (+ eval reg v) r reg}
        Just (Mul r v) -> go $ state' {registers = M.adjust (* eval reg v) r reg}
        Just (Mod r v) -> go $ state' {registers = M.adjust (`mod` eval reg v) r reg}
        Just (Jgz r v)
          | 0 < eval reg r -> go $ state {pc = c + eval reg v}
          | otherwise -> go state'
        Just (Snd o) -> go $ state' {frequency = eval reg o}
        Just (Rcv r)
          | 0 < M.findWithDefault 0 r reg -> go $ state' {recovered = freq}
          | otherwise -> go state'
      where
        state' = state {pc = c + 1}

eval :: M.Map Char Int -> Operand -> Int
eval reg op = case op of
  Literal l -> l
  Register r -> M.findWithDefault 0 r reg

runProgram2 :: M.Map Int Instruction -> Int
runProgram2 prog = go (createCpu 0) (createCpu 1)
  where
    createCpu i = Cpu i (M.singleton 'p' i) Running 0 [] 0
    locked cpu = state cpu == Waiting && null (inbox cpu)
    doneorl cpu = locked cpu || state cpu == Done
    receive cpu@(Cpu i reg st c ibox s) = case prog M.!? c of
      (Just (Rcv r)) -> cpu {state = Running, inbox = tail ibox, cregisters = M.insert r (head ibox) reg, pco = c + 1}
      _ -> undefined
    go c1@(Cpu i reg st c ibox s) c2
      | doneorl c1 && doneorl c2 = sent $ if 1 == cid c1 then c1 else c2
      | doneorl c1 = go c2 c1
      | st == Waiting = go (receive c1) c2
      | otherwise = go c1' c2'
      where
        eval' o = eval reg o
        c1'' = c1 {pco = c + 1}
        (c1', c2') = case prog M.!? c of
          Nothing -> (c1, c2)
          Just (Set r v) -> (c1'' {cregisters = M.insert r (eval' v) reg}, c2)
          Just (Add r v) -> (c1'' {cregisters = M.adjust (+ eval' v) r reg}, c2)
          Just (Mul r v) -> (c1'' {cregisters = M.adjust (* eval' v) r reg}, c2)
          Just (Mod r v) -> (c1'' {cregisters = M.adjust (`mod` eval' v) r reg}, c2)
          Just (Jgz r v)
            | 0 < eval' r -> (c1 {pco = c + eval' v}, c2)
            | otherwise -> (c1'', c2)
          Just (Snd o) -> (c1'' {sent = 1 + s}, c2 {inbox = inbox c2 <> [eval' o]})
          Just (Rcv r)
            | null ibox -> (c2, c1 {state = Waiting})
            | otherwise -> (receive c1, c2)

-- >>>  solve $ parse "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a\njgz a -1\nset a 1\njgz a -2"
-- >>>  solve $ parse "snd 1\nsnd 2\nsnd p\nrcv a\nrcv b\nrcv c\nrcv d"
-- (4,1)
-- (0,3)

solve :: [Instruction] -> (Int, Int)
solve l = (p1, p2)
  where
    prog = M.fromAscList $ zip [0 ..] l
    p1x = runProgram prog (\s -> recovered s > 0)
    p1 = recovered $ runProgram prog ((> 0) . recovered)
    p2 = runProgram2 prog

main :: IO ()
main = readFile "input/day18.txt" >>= print . solve . parse
