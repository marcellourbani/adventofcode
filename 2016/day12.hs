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

data Instruction = CPY Operand Register | INC Register | DEC Register | JNZ Operand Int deriving (Eq, Show)

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
      <|> try (symbol "jnz" *> (JNZ <$> parseOperand <*> L.signed sc L.decimal))
  where
    parseRegister = lexeme $ (A <$ symbol "a") <|> (B <$ symbol "b") <|> (C <$ symbol "c") <|> (D <$ symbol "d")
    parseOperand = lexeme $ (Lit <$> L.signed sc L.decimal) <|> (Reg <$> parseRegister)

execute :: CPU -> Maybe CPU
execute (CPU pc regs prog) = case M.lookup pc prog of
  Nothing -> Nothing
  Just i -> Just $ CPU pc' regs' prog
    where
      regs' = case i of
        CPY o r -> M.insert r (ov o) regs
        INC r -> M.adjust (+ 1) r regs
        DEC r -> M.adjust (subtract 1) r regs
        _ -> regs

      pc' = case i of
        JNZ o n | ov o /= 0 -> pc + n
        _ -> pc + 1

      ov o = case o of
        Lit i -> i
        Reg r -> M.findWithDefault 0 r regs

-- >>> solve  $ parse "cpy 41 a\ninc a\ninc a\ndec a\njnz a 2\ndec a"
-- 42
solve :: [Instruction] -> (Int, Int)
solve l = (go initial, go initial2)
  where
    iregs = M.fromList $ zip [A, B, C, D] $ repeat 0
    initial = CPU 0 iregs $ M.fromList $ zip [0 ..] l
    initial2 = CPU 0 (M.insert C 1 iregs) $ M.fromList $ zip [0 ..] l
    go c@(CPU _ r _) = case execute c of
      Nothing -> M.findWithDefault 0 A r
      Just c' -> go c'

main :: IO ()
main = readFile "input/day12.txt" >>= print . solve . parse
