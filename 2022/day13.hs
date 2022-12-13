#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant compare" #-}

module Main where

import Data.Either (fromRight)
import Data.List (elemIndex, sort)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, empty, many, runParser, some, try, (<|>))
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char as LC
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser () -- consume spaces
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

data Packet = Imm Int | Nest [Packet] deriving (Show, Eq)

instance Ord Packet where
  compare a b
    | a == b = EQ
    | otherwise = case (a, b) of
      (Imm x, Imm y) -> compare x y
      (Imm _, Nest _) -> compare (Nest [a]) b
      (Nest _, Imm _) -> compare a $ Nest [b]
      (Nest [], Nest _) -> LT
      (Nest _, Nest []) -> GT
      (Nest (x : xs), Nest (y : ys))
        | compare x y == EQ -> compare (Nest xs) (Nest ys)
        -- slight inconsistency with derived EQ -  works anyway for this use case as we don't use ==
        --  to fix I should implement manually with Nest [Imm a] == Imm a
        -- now compare (Imm 1) (Nest [Imm 1]) -> EQ
        -- (Imm 1) == (Nest [Imm 1]) -> False
        | otherwise -> compare x y

--  >>> Nest [Imm 7,Imm 7,Imm 7,Imm 7] < Nest [Imm 7,Imm 7,Imm 8]
--  >>> Nest [Imm 7] < Nest [Imm 7,Imm 7,Imm 8]
-- True
-- True

parseP :: Parser Packet
parseP =
  Imm <$> L.decimal
    <|> Nest <$> nested (list parseP)
  where
    nested = between (string "[") (string "]")
    list p = many (try (p <* symbol ",") <|> p)

parseInput :: Parser [(Packet, Packet)]
parseInput = many (try (parseC <* eol <* eol) <|> parseC) where parseC = (,) <$> (parseP <* eol) <*> parseP

parse :: String -> [(Packet, Packet)]
parse s = fromRight [] $ runParser parseInput "" s

-- >>> solve $ parse "[1,1,3,1,1]\n[1,1,5,1,1]\n\n[[1],[2,3,4]]\n[[1],4]\n\n[9]\n[[8,7,6]]\n\n[[4,4],4,4]\n[[4,4],4,4,4]\n\n[7,7,7,7]\n[7,7,7]\n\n[]\n[3]\n\n[[[]]]\n[[]]\n\n[1,[2,[3,[4,[5,6,7]]]],8,9]\n[1,[2,[3,[4,[5,6,0]]]],8,9]"
-- (13,140)

solve :: [(Packet, Packet)] -> (Int, Int)
solve l = (p1, p2)
  where
    p1 = sum [n | (n, c) <- zip [1 ..] $ uncurry (<) <$> l, c]
    p2 = idx m1 * idx m2
    idx m = 1 + fromMaybe 0 (elemIndex m allpackets)
    (m1, m2) = head $ parse "[[2]]\n[[6]]"
    allpackets = sort $ [m1, m2] <> (fst <$> l) <> (snd <$> l)

main :: IO ()
main = readFile "input/day13.txt" >>= print . solve . parse
