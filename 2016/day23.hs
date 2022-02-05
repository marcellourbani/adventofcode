#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Either (fromRight)
import Data.Foldable (Foldable (foldl'))
import qualified Data.Map.Strict as M
import Data.Void (Void)
import Text.Megaparsec (Parsec, empty, runParser, some, try, (<|>))
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Register = A | B | C | D deriving (Eq, Ord, Show, Enum)

data Operand = Lit Int | Reg Register deriving (Eq, Ord, Show)

data Instruction
  = CPY Operand Register
  | JNZ Operand Operand
  | INC Register
  | DEC Register
  | TGL Register
  | Nop
  deriving (Eq, Show)

data CPU = CPU Int (M.Map Register Int) (M.Map Int Instruction) deriving (Eq, Show)

type Parser = Parsec Void String

sc :: Parser () -- consume spaces
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parse :: String -> [Instruction]
parse s = fromRight [] $ runParser (some parseInstruction) "" s

parseInstruction :: Parser Instruction
parseInstruction =
  lexeme $
    try (symbol "cpy" *> (CPY <$> parseOperand <*> parseRegister))
      <|> try (symbol "inc" *> (INC <$> parseRegister))
      <|> try (symbol "dec" *> (DEC <$> parseRegister))
      <|> try (symbol "jnz" *> (JNZ <$> parseOperand <*> parseOperand))
      <|> try (symbol "tgl" *> (TGL <$> parseRegister))
  where
    parseRegister = lexeme $ (A <$ symbol "a") <|> (B <$ symbol "b") <|> (C <$ symbol "c") <|> (D <$ symbol "d")
    parseOperand = lexeme $ (Lit <$> L.signed sc L.decimal) <|> (Reg <$> parseRegister)

execute :: CPU -> Maybe CPU
execute (CPU pc regs prog) = case M.lookup pc prog of
  Nothing -> Nothing
  Just i -> Just $ CPU pc' regs' prog'
    where
      regs' = case i of
        CPY o r -> M.insert r (ov o) regs
        INC r -> M.adjust (+ 1) r regs
        DEC r -> M.adjust (subtract 1) r regs
        _ -> regs

      tgl r = case fixed of
        Nothing -> prog
        Just (ix, i) -> M.insert ix i prog
        where
          fixed = case ins of
            Nothing -> Nothing
            Just (TGL s) -> Just (idx, INC s)
            Just (INC s) -> Just (idx, DEC s)
            Just (DEC s) -> Just (idx, INC s)
            Just (JNZ s (Reg n)) -> Just (idx, CPY s n)
            Just (CPY s n) -> Just (idx, JNZ s $ Reg n)
            Just Nop -> Just (idx, Nop)
            _ -> Nothing
          idx = pc + ov (Reg r)
          ins = M.lookup idx prog

      prog' = case i of
        (TGL r) -> tgl r
        _ -> prog

      pc' = case i of
        JNZ o n | ov o /= 0 -> pc + ov n
        _ -> pc + 1

      ov o = case o of
        Lit i -> i
        Reg r -> M.findWithDefault 0 r regs

-- >>> solve  $ parse "cpy 2 a\ntgl a\ntgl a\ntgl a\ncpy 1 a\ndec a\ndec a"
-- (3,3)

solve :: [Instruction] -> (Int, Int)
solve l = (go initial, go initial2)
  where
    iregs = M.fromList $ zip [A, B, C, D] $ 7 : repeat 0
    initial = CPU 0 iregs $ M.fromList $ zip [0 ..] l
    initial2 = CPU 0 (M.insert A 12 iregs) $ M.fromList $ zip [0 ..] l
    go c@(CPU _ r _) = case execute c of
      Nothing -> M.findWithDefault 0 A r
      Just c' -> go c'

main :: IO ()
main = readFile "input/day23.txt" >>= print . solve . parse
