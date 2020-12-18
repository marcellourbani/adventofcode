#!/usr/bin/env stack
-- stack --resolver lts-10.2 script

{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad.Combinators (choice)
import Control.Monad.Combinators.Expr
import Data.Either (fromRight)
import Data.Void
import Text.Megaparsec
  ( Parsec,
    between,
    empty,
    many,
    runParser,
    some,
    (<?>),
    (<|>),
  )
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Operation = Sum | Multiply deriving (Show)

data Expression = Leaf Int | Op Operation Expression Expression | Inner Expression
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

infixL :: String -> (Expression -> Expression -> Expression) -> Operator Parser Expression
infixL name f = InfixL (f <$ symbol name)

operatorTable :: [[Operator Parser Expression]]
operatorTable = [[infixL "+" (Op Sum), infixL "*" (Op Multiply)]]

expParser :: Parser Expression
expParser = makeExprParser pTerm operatorTable

parseInput = runParser expParser ""

eval :: Expression -> Int
eval e = case e of
  Leaf n -> n
  Op Sum e1 e2 -> eval e1 + eval e2
  Op Multiply e1 e2 -> eval e1 * eval e2
  Inner e1 -> eval e1

-- >>> (\x->(eval x,x)) <$> parseInput "3+2*3"
-- Right (15,Op Multiply (Op Sum (Leaf 3) (Leaf 2)) (Leaf 3))

-- >>> solve "2 * 3 + (4 * 5)\n5 + (8 * 3 + 9 + 3 * 4 * 3)"
-- (Right 463,2)

solve :: String -> (Int, Int)
solve s = (first, second)
  where
    first = fromRight 0 results
    second = 2
    results = sum . fmap eval <$> input
    input = traverse (runParser expParser "") $ lines s

-- >>> solve <$> readFile "input/day18.txt"
-- (800602729153,2)

main :: IO ()
main = readFile "input/day18.txt" >>= print . solve