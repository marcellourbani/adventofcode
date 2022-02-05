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

data Instruction = CPY Operand Register | INC Register | DEC Register | JNZ Operand Int | OUT Register deriving (Eq, Show)

data CPU = CPU Int (M.Map Register Int) (M.Map Int Instruction) [Int] deriving (Eq, Show)

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
      <|> try (symbol "out" *> (OUT <$> parseRegister))
  where
    parseRegister = lexeme $ (A <$ symbol "a") <|> (B <$ symbol "b") <|> (C <$ symbol "c") <|> (D <$ symbol "d")
    parseOperand = lexeme $ (Lit <$> L.signed sc L.decimal) <|> (Reg <$> parseRegister)

execute :: CPU -> Maybe CPU
execute (CPU pc regs prog out) = case M.lookup pc prog of
  Nothing -> Nothing
  Just i -> Just $ CPU pc' regs' prog out'
    where
      regs' = case i of
        CPY o r -> M.insert r (ov o) regs
        INC r -> M.adjust (+ 1) r regs
        DEC r -> M.adjust (subtract 1) r regs
        _ -> regs

      out' = case i of
        OUT r -> ov (Reg r) : out
        _ -> out

      pc' = case i of
        JNZ o n | ov o /= 0 -> pc + n
        _ -> pc + 1

      ov o = case o of
        Lit i -> i
        Reg r -> M.findWithDefault 0 r regs

-- >>> solve  $ parse "cpy a d\ncpy 15 c\ncpy 170 b\ninc d\ndec b\njnz b -2\ndec c\njnz c -5\ncpy d a\njnz 0 0\ncpy a b\ncpy 0 a\ncpy 2 c\njnz b 2\njnz 1 6\ndec b\ndec c\njnz c -4\ninc a\njnz 1 -7\ncpy 2 b\njnz c 2\njnz 1 4\ndec b\ndec c\njnz 1 -4\njnz 0 0\nout b\njnz a -19\njnz 1 -21"
-- 180

-- could have simply taken the integers form line 2 and 3 of the input, but the stuff above helped the analysis
solve :: [Instruction] -> Int
solve l = go2 1 delta - delta
  where
    iregs = M.fromList $ zip [A, B, C, D] $ repeat 0
    initial = CPU 0 iregs (M.fromList (zip [0 ..] (take 3 l))) []
    r = go initial
    delta = M.findWithDefault 0 B r * M.findWithDefault 0 C r
    go2 n target
      | target <= 2 ^ n = 0
      | otherwise = 2 ^ n + go2 (n + 2) target
    go c@(CPU _ r _ o) = case execute c of
      Nothing -> r
      Just c'@(CPU _ _ _ a) -> go c'

main :: IO ()
main = readFile "input/day25.txt" >>= print . solve . parse
