#!/usr/bin/env stack
-- stack --resolver lts-10.2 script

{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.Either (rights)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Rule = RAtom Char | RSeq [[Int]] deriving (Show)

data Message = Message (M.Map Int Rule) [String] deriving (Show)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

pInt :: Parser Int
pInt = lexeme L.decimal

pRuleElem :: Parser [Int]
pRuleElem = many pInt

pCharAtom :: Parser Rule
pCharAtom = lexeme $ RAtom <$> between (char '"') (char '"') (choice [char 'a', char 'b'])

pRule :: Parser Rule
pRule =
  lexeme $
    choice
      [ pCharAtom,
        RSeq <$> pRuleElems
      ]

pRuleLine :: Parser (Int, Rule)
pRuleLine = do
  idx <- pInt
  _ <- lexeme $ char ':'
  rule <- pRule
  return (idx, rule)

pRuleElems :: Parser [[Int]]
pRuleElems =
  lexeme $
    try
      ( do
          rule <- pRuleElem
          _ <- lexeme $ char '|'
          rules <- pRuleElems
          return $ rule : rules
      )
      <|> ((: []) <$> pRuleElem <* eof)

pInput :: String -> Message
pInput s = Message rules $ lines $ rs !! 1
  where
    rs = splitOn "\n\n" s
    rules = either (const M.empty) M.fromList $ traverse (parse pRuleLine "") $ lines $ head rs

createParser :: M.Map Int Rule -> Int -> Parser String
createParser m i = go i <* eof
  where
    go :: Int -> Parser String
    go k = case M.lookup k m of
      Nothing -> undefined
      Just (RAtom c) -> string [c]
      Just (RSeq seqs) -> choice $ try . seqParser . fmap go <$> seqs
    seqParser = foldl1 (\x y -> (++) <$> x <*> y)

-- >>> solve "0: 4 1 5\n1: 2 3 | 3 2\n2: 4 4 | 5 5\n3: 4 5 | 5 4\n4: \"a\"\n5: \"b\"\n\nababbb\nbababa\nabbbab\naaabbb\naaaabbb"
-- (["ababbb","abbbab"],5)

solve :: String -> (Int, Int)
solve s = (first, second)
  where
    first = length $ rights $ runParser sparser "" <$> source
    second = length source
    sparser = createParser rules 0
    Message rules source = pInput s

main :: IO ()
main = readFile "input/day19.txt" >>= print . solve
