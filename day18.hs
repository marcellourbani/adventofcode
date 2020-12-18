#!/usr/bin/env stack
-- stack --resolver lts-10.2 script

{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad.Combinators (choice)
import Control.Monad.Combinators.Expr
import Data.Either (fromRight)
import Data.Void
import Text.Megaparsec (Parsec, between, empty, runParser, some)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Expression
  = Leaf Int
  | Sum Expression Expression
  | Multiply Expression Expression
  | Inner Expression
  deriving (Show)

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- >>> runParser sc "" " 3 * 1"
-- Right ()
pInt :: Parser Expression
pInt = Leaf <$> lexeme L.decimal

infixL :: String -> (Expression -> Expression -> Expression) -> Operator Parser Expression
infixL name f = InfixL (f <$ symbol name)

infixN :: String -> (Expression -> Expression -> Expression) -> Operator Parser Expression
infixN name f = InfixN (f <$ symbol name)

intParser :: Parser Expression
intParser = L.lexeme sc $ do
  s <- some digitChar
  return $ Leaf $ read s

pTerm :: Parser Expression
pTerm =
  choice
    [ pInt,
      between (symbol "(") (symbol ")") expParser
    ]

operatorTable :: [[Operator Parser Expression]]
operatorTable =
  [ [infixL "+" Sum, infixL "*" Multiply]
  ]

expParser :: Parser Expression
expParser = makeExprParser pTerm operatorTable

pTerm2 :: Parser Expression
pTerm2 =
  choice
    [ pInt,
      between (symbol "(") (symbol ")") expParser2
    ]

operatorTable2 :: [[Operator Parser Expression]]
operatorTable2 =
  [ [infixL "+" Sum],
    [infixL "*" Multiply]
  ]

expParser2 :: Parser Expression
expParser2 = makeExprParser pTerm2 operatorTable2

eval :: Expression -> Int
eval e = case e of
  Leaf n -> n
  Sum e1 e2 -> eval e1 + eval e2
  Multiply e1 e2 -> eval e1 * eval e2
  Inner e1 -> eval e1

pp :: Expression -> String
pp e = case e of
  Leaf n -> show n
  Sum e1 e2 -> "(" ++ pp e1 ++ "+" ++ pp e2 ++ ")"
  Multiply e1 e2 -> '(' : pp e1 ++ "*" ++ pp e2 ++ ")"
  Inner e1 -> "(" ++ pp e1 ++ ")"

-- >>> solve "2 * 3 + (4 * 5)\n5 + (8 * 3 + 9 + 3 * 4 * 3)"
-- (463,1491)

solve :: String -> (Int, Int)
solve s = (first, second)
  where
    first = fromRight 0 results
    second = fromRight 0 results2
    results = sum . fmap eval <$> input
    input = traverse (runParser expParser "") $ lines s
    results2 = sum . fmap eval <$> input2
    input2 = traverse (runParser expParser2 "") $ lines s

main :: IO ()
main = readFile "input/day18.txt" >>= print . solve