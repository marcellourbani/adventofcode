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
  | Rcv Operand
  | Jgz Operand Operand
  deriving (Show)

data State = State {registers :: M.Map Char Int, frequency :: Int, recovered :: Int, pc :: Int} deriving (Show)

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
    <|> (symbol "rcv" *> (Rcv <$> op))
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
          Just (Set r v) -> go $ state' {registers = M.insert r (eval v) reg}
          Just (Add r v) -> go $ state' {registers = M.adjust (+ eval v) r reg}
          Just (Mul r v) -> go $ state' {registers = M.adjust (* eval v) r reg}
          Just (Mod r v) -> go $ state' {registers = M.adjust (`mod` eval v) r reg}
          Just (Jgz r v)
            | 0 < eval r -> go $ state {pc = c + eval v}
            | otherwise -> go state'
          Just (Snd o) -> go $ state' {frequency = eval o}
          Just (Rcv o)
            | 0 < eval o -> go $ state' {recovered = freq}
            | otherwise -> go state'
      where
        state' = state {pc = c + 1}
        regval r = M.findWithDefault 0 r reg
        eval op = case op of
          Literal l -> l
          Register r -> regval r

-- >>>  solve $ parse "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a\njgz a -1\nset a 1\njgz a -2"
-- (4,4)
solve :: [Instruction] -> (Int, Int)
solve l = (p1, p1)
  where
    prog = M.fromAscList $ zip [0 ..] l
    p1x = runProgram prog (\s -> recovered s > 0)
    p1 = recovered $ runProgram prog ((> 0) . recovered)

main :: IO ()
main = readFile "input/day18.txt" >>= print . solve . parse
