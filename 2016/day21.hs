#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Either (fromRight)
import Data.Foldable (fold, foldl')
import Data.Functor ((<&>))
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Debug.Trace
import Text.Megaparsec (Parsec, empty, runParser, some, try, (<|>))
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L

data Operation
  = SwapPos Int Int
  | SwapLetter Char Char
  | RotateLeft Int
  | RotateRight Int
  | RotateLetter Char
  | ReversePos Int Int
  | MovePos Int Int
  deriving (Show)

type Parser = Parsec Void String

sc :: Parser () -- consume spaces
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parse :: String -> [Operation]
parse s = fromRight [] $ runParser (some parseOp) "" s

parseOp :: Parser Operation
parseOp =
  try
    (SwapPos <$> (symbol "swap" *> pos) <*> wp pos)
    <|> try (SwapLetter <$> (symbol "swap" *> lett) <*> wp lett)
    <|> try (RotateRight <$> (symbol "rotate" *> symbol "right" *> lexeme L.decimal) <* ss)
    <|> try (RotateLeft <$> (symbol "rotate" *> symbol "left" *> lexeme L.decimal) <* ss)
    <|> try (ReversePos <$> (symbol "reverse" *> symbol "positions" *> lexeme L.decimal) <*> (symbol "through" *> lexeme L.decimal))
    <|> try (MovePos <$> (symbol "move" *> pos) <*> (symbol "to" *> pos))
    <|> try (RotateLetter <$> (fold (symbol <$> ["rotate", "based", "on", "position", "of"]) *> lett))
  where
    pos = symbol "position" *> lexeme L.decimal
    lett = symbol "letter" *> lexeme L.charLiteral
    wp p = symbol "with" *> p
    ss = symbol "steps" <|> symbol "step"

runOp :: String -> Operation -> String
runOp s o = case o of
  SwapPos a b -> sn a b
  SwapLetter a b -> fromMaybe s $ sn <$> elemIndex a s <*> elemIndex b s
  RotateLeft a -> drop a s <> take a s
  RotateRight a -> rr a
  RotateLetter a -> case elemIndex a s of
    Nothing -> s
    Just b -> rr $ if b >= 4 then b + 2 else b + 1
  ReversePos a b -> x <> reverse y <> z where (x, y, z) = sp a (b + 1)
  MovePos a b -> take b r <> (s !! a : drop b r) where r = take a s <> drop (a + 1) s
  where
    rr l = drop l' s <> take l' s where lm = mod l $length s; l' = length s - lm
    sp x y = (take a s, drop a $take b s, drop b s) where a = min x y; b = max x y
    sn x y
      | x == y = s
      | otherwise = prea <> [s !! max x y] <> tail mid <> [s !! min x y] <> tail postb
      where
        (prea, mid, postb) = sp x y

-- reverse each operation
-- Another strategy could be to reverse position mappings
-- so if char 1 is moved to position 3 in the direct translation, do the opposite in the reversed translation
reverseOp :: String -> Operation -> String
reverseOp s o = case o of
  RotateLeft a -> runOp s $ RotateRight a
  RotateRight a -> runOp s $ RotateLeft a
  RotateLetter a -> case elemIndex a s of
    Nothing -> s
    Just b -> runOp s $ RotateLeft $ reverserot (length s) b
  MovePos a b -> runOp s $ MovePos b a
  _ -> runOp s o
  where
    reverserot m n
      | n == 0 = 1
      | odd n = div n 2 + 1
      | otherwise = mod (div (n + m -2) 2 + 2) m

solve :: String -> String -> [Operation] -> (String, String)
solve s t l = (foldl' runOp s l, foldl' reverseOp t $ reverse l)

main :: IO ()
main = readFile "input/day21.txt" >>= print . solve "abcdefgh" "fbgdceah" . parse
