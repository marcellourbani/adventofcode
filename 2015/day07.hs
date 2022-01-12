#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE TupleSections #-}

module Main where

import Data.Binary (Word16)
import Data.Bits
import Data.Either (fromRight, rights)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Void (Void)
import Text.Megaparsec (Parsec, empty, many, runParser, some, try, (<|>))
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char as LC
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Operand = Lit Word16 | Var String
  deriving (Show, Eq)

data GateOp
  = Set Operand
  | And Operand Operand
  | Or Operand Operand
  | LShift Operand Int
  | RShift Operand Int
  | Not Operand
  deriving (Show, Eq)

data Gate = Gate GateOp String deriving (Show, Eq)

sc :: Parser () -- consume spaces
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parse :: String -> [Gate]
parse i = fromRight [] $ traverse (runParser gate "") $ lines i

identifier :: Parser String
identifier = lexeme $ (:) <$> LC.letterChar <*> many LC.alphaNumChar

operand :: Parser Operand
operand = lexeme $ (L.decimal <&> Lit) <|> Var <$> identifier

gate :: Parser Gate
gate = Gate <$> gateOp <*> lexeme (symbol "->" *> identifier)

foo = lexeme (symbol "->" *> identifier)

gateOp :: Parser GateOp
gateOp =
  try (Not <$> op "NOT")
    <|> try (And <$> operand <*> op "AND")
    <|> try (Or <$> operand <*> op "OR")
    <|> try (LShift <$> operand <*> ops "LSHIFT")
    <|> try (RShift <$> operand <*> ops "RSHIFT")
    <|> try (Set <$> operand)
  where
    op s = lexeme $ symbol s *> operand
    ops s = lexeme $ symbol s *> L.decimal

simulate :: [Gate] -> M.Map String Word16 -> M.Map String Word16
simulate = go
  where
    go c m = case sets of
      [] -> m
      _ -> go c' m'
      where
        sets = catMaybes $ eval <$> c
        m' = M.union m $ M.fromList sets
        c' = filter missingw c
        missingw (Gate _ t) = M.notMember t m'
        eval (Gate gop t) =
          (t,) <$> case gop of
            Set o -> opval o
            Not o -> complement <$> opval o
            And o1 o2 -> (.&.) <$> opval o1 <*> opval o2
            Or o1 o2 -> (.|.) <$> opval o1 <*> opval o2
            LShift o i -> shiftL <$> opval o <*> Just i
            RShift o i -> shiftR <$> opval o <*> Just i

        opval op = case op of
          Lit i -> Just i
          Var v -> M.lookup v m

-- >>> solve $ parse "123 -> x\n456 -> y\nx AND y -> d\nx OR y -> e\nx LSHIFT 2 -> f\ny RSHIFT 2 -> g\nNOT x -> h\nNOT y -> i\nf -> a"
-- (492,492)

solve :: [Gate] -> (Word16, Word16)
solve l = (p1, p2)
  where
    p1 = simulate l M.empty M.! "a"
    p2 = simulate l (M.singleton "b" p1) M.! "a"

main :: IO ()
main = readFile "input/day07.txt" >>= print . solve . parse
