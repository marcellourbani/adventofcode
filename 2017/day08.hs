#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Control.Applicative ((<**>))
import Data.Bifunctor (first)
import Data.Either (fromRight)
import Data.Foldable (fold, foldl')
import Data.Functor ((<&>))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (lookAhead, notFollowedBy), Parsec, anySingle, between, count, empty, manyTill, runParser, some, try, (<|>))
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char as LC
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Cond
  = CGT String Int
  | CLT String Int
  | CEQ String Int
  | CNE String Int
  | CGE String Int
  | CLE String Int
  deriving (Show, Eq)

data Instruction = Inc String Int Cond | Dec String Int Cond deriving (Show, Eq)

type CPU = M.Map String Int

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
  try (Inc <$> parseReg <* symbol "inc" <*> parseInt <*> parseCond)
    <|> try (Dec <$> parseReg <* symbol "dec" <*> parseInt <*> parseCond)
  where
    parseCond = (symbol "if" *> parseReg) <**> parseSign <*> parseInt
    parseInt = L.signed sc $ lexeme L.decimal
    parseReg = lexeme $ some LC.letterChar
    parseSign =
      lexeme $
        try (CGE <$ symbol ">=")
          <|> try (CLE <$ symbol "<=")
          <|> try (CGT <$ symbol ">")
          <|> try (CLT <$ symbol "<")
          <|> try (CEQ <$ symbol "==")
          <|> try (CNE <$ symbol "!=")

exec :: CPU -> Instruction -> CPU
exec cpu i = case i of
  Inc reg v c | valid c -> M.unionWith (+) cpu $ M.singleton reg v
  Dec reg v c | valid c -> M.unionWith (+) cpu $ M.singleton reg (- v)
  _ -> cpu
  where
    rv r = M.findWithDefault 0 r cpu
    valid c = case c of
      CGT r v -> rv r > v
      CLT r v -> rv r < v
      CEQ r v -> rv r == v
      CNE r v -> rv r /= v
      CGE r v -> rv r >= v
      CLE r v -> rv r <= v

-- >>> solve $ parse "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10"
-- (1,10)
solve :: [Instruction] -> (Int, Int)
solve l = first maximum s
  where
    s = foldl' f (M.empty, 0) l
    f (cpu, m) i = (cpu', max m mc)
      where
        cpu' = exec cpu i
        mc = if M.null cpu' then 0 else maximum cpu'

main :: IO ()
main = readFile "input/day08.txt" >>= print . solve . parse
