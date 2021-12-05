#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Void
import Text.Megaparsec (ParseErrorBundle, Parsec, between, empty, runParser, some)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Point = Point {x :: Int, y :: Int}
  deriving (Show, Eq)

data Vector = Vector {start :: Point, end :: Point}
  deriving (Show, Eq)

type Parser = Parsec Void String

sc :: Parser () -- consume spaces
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

pointParser :: Parser Point
pointParser = Point <$> lexeme L.decimal <* lexeme (char ',') <*> lexeme L.decimal

vectorParser :: Parser Vector
vectorParser = Vector <$> pointParser <* lexeme (string "->") <*> pointParser

-- >>> fmap solve $ parse [ "0,9 -> 5,9", "8,0 -> 0,8", "9,4 -> 3,4", "2,2 -> 2,1", "7,0 -> 7,4", "6,4 -> 2,0", "0,9 -> 2,9", "3,4 -> 1,4", "0,0 -> 8,8", "5,5 -> 8,2" ]
-- Right 1
solve :: [Vector] -> (Int, Int)
solve _ = (0, 0)

-- >>> parse "0,9 -> 5,9"
-- 2
parse :: [String] -> Either (ParseErrorBundle String Void) [Vector]
parse = traverse (runParser vectorParser "")

main :: IO ()
main = readFile "input/day5.txt" >>= print . fmap solve . parse . lines
