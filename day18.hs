#!/usr/bin/env stack
-- stack --resolver lts-10.2 script

{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad.Combinators (choice)
import Control.Monad.Combinators.Expr
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
operatorTable = [[infixL "+" (Op Sum), infixL "*" (Op Sum)]]

expParser :: Parser Expression
expParser = makeExprParser pTerm operatorTable

-- >>> runParser expParser "" "2+1 +3+ 4"
-- Right (Op Sum (Op Sum (Op Sum (Leaf 2) (Leaf 1)) (Leaf 3)) (Leaf 4))

-- >>> solve "2 * 3 + (4 * 5)"
-- (Right (Op Sum (Op Sum (Leaf 2) (Leaf 3)) (Op Sum (Leaf 4) (Leaf 5))),2)

-- solve :: String -> (Int, Int)
solve s = (first, second)
  where
    first = input
    second = 2
    input = runParser expParser "" s

main :: IO ()
main = readFile "input/day17.txt" >>= print . solve