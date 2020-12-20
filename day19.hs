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

createParser2 :: M.Map Int Rule -> Int -> Parser String
createParser2 m i = go i <* eof
  where
    go :: Int -> Parser String
    go k = case M.lookup k m of
      Nothing -> undefined
      Just (RAtom c) -> string [c]
      Just (RSeq seqs)
        | k == 8 -> choice [times n [p42] | n <- [1 .. 100]]
        | k == 11 -> choice [times n [p42, p31] | n <- [1 .. 100]]
        | otherwise -> choice $ try . seqParser . fmap go <$> seqs
        where
          p42 = go 42
          p31 = go 31
          times n l = try $ seqParser $ foldl1 (++) $ replicate n <$> l
    seqParser = foldl1 (\x y -> (++) <$> x <*> y)

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

-- >>> solve "42: 9 14 | 10 1\n9: 14 27 | 1 26\n10: 23 14 | 28 1\n1: \"a\"\n11: 42 31\n5: 1 14 | 15 1\n19: 14 1 | 14 14\n12: 24 14 | 19 1\n16: 15 1 | 14 14\n31: 14 17 | 1 13\n6: 14 14 | 1 14\n2: 1 24 | 14 4\n0: 8 11\n13: 14 3 | 1 12\n15: 1 | 14\n17: 14 2 | 1 7\n23: 25 1 | 22 14\n28: 16 1\n4: 1 1\n20: 14 14 | 1 15\n3: 5 14 | 16 1\n27: 1 6 | 14 18\n14: \"b\"\n21: 14 1 | 1 14\n25: 1 1 | 1 14\n22: 14 14\n8: 42\n26: 14 22 | 1 20\n18: 15 15\n7: 14 5 | 1 21\n24: 14 1\n\nabbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa\nbbabbbbaabaabba\nbabbbbaabbbbbabbbbbbaabaaabaaa\naaabbbbbbaaaabaababaabababbabaaabbababababaaa\nbbbbbbbaaaabbbbaaabbabaaa\nbbbababbbbaaaaaaaabbababaaababaabab\nababaaaaaabaaab\nababaaaaabbbaba\nbaabbaaaabbaaaababbaababb\nabbbbabbbbaaaababbbbbbaaaababb\naaaaabbaabaaaaababaa\naaaabbaaaabbaaa\naaaabbaabbaaaaaaabbbabbbaaabbaabaaa\nbabaaabbbaaabaababbaabababaaab\naabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"
-- (3,6)

solve :: String -> (Int, Int)
solve s = (first, second)
  where
    first = length $ rights $ runParser sparser "" <$> source
    second = length $ rights $ runParser sparser2 "" <$> source
    sparser = createParser rules 0
    sparser2 = createParser rules2 0
    repeats n l = [foldl1 (++) $ replicate i <$> l | i <- [1 .. n]]
    rules2 = M.insert 11 (RSeq $ repeats 100 [42, 31]) $ M.insert 8 (RSeq $ repeats 100 [42]) rules
    Message rules source = pInput s

main :: IO ()
main = readFile "input/day19.txt" >>= print . solve

-- 35: "a"
-- 72: "b"
-- 42: 72 122 | 35 29
-- 31: 35 62 | 72 54
-- 8: 42 | 42 8
-- 11: 42 31 | 42 11 31