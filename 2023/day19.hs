#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import Data.Either (fromRight)
import Data.Functor (($>))
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, empty, many, manyTill, runParser, some, try, (<|>))
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char as LC
import qualified Text.Megaparsec.Char.Lexer as L

type Part = M.Map String Int

data Destination = A | R | W String deriving (Show)

data Workfrow = Workfrow {wfName :: String, wfRules :: [(String, Ordering, Int, Destination)], wfDefault :: Destination} deriving (Show)

type Parser = Parsec Void String

sc :: Parser () -- consume spaces
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parseDest :: Parser Destination
parseDest = (symbol "A" $> A) <|> (symbol "R" $> R) <|> (W <$> many LC.alphaNumChar)

parseRule :: Parser (String, Ordering, Int, Destination)
parseRule = do
  component <- many LC.alphaNumChar
  s <- (symbol "<" $> LT) <|> (symbol ">" $> GT)
  n <- L.decimal
  _ <- symbol ":"
  d <- parseDest
  return (component, s, n, d)

parseWorkflow :: Parser Workfrow
parseWorkflow = Workfrow <$> many LC.alphaNumChar <*> (symbol "{" *> many (try parseRule <* symbol ",")) <*> (parseDest <* symbol "}")

parseList :: Parser a -> Parser [a]
parseList p = many (try (p <* symbol ",") <|> p)

parsePart :: Parser Part
parsePart = M.fromList <$> lexeme (char '{' *> parseList ((,) <$> many LC.alphaNumChar <*> (symbol "=" *> L.decimal)) <* char '}')

parse :: String -> (M.Map String Workfrow, [Part])
parse s = (M.fromList $ zip (wfName <$> wfs) wfs, pl parsePart ps)
  where
    [wf, ps] = splitOn "\n\n" s
    wfs = pl parseWorkflow wf
    pl p i = fromRight [] $ runParser (many p) "" i

isAccepted :: M.Map String Workfrow -> Part -> Bool
isAccepted wfs part = go $ W "in"
  where
    dest rs def = case rs of
      [] -> def
      (n, LT, v, d) : _ | part M.! n < v -> d
      (n, GT, v, d) : _ | part M.! n > v -> d
      _ : rs' -> dest rs' def
    go d = case d of
      A -> True
      R -> False
      W n -> case M.lookup n wfs of
        Nothing -> False
        Just (Workfrow _ rs def) -> go $ dest rs def

-- >>> solve $ parse "px{a<2006:qkq,m>2090:A,rfg}\npv{a>1716:R,A}\nlnx{m>1548:A,A}\nrfg{s<537:gd,x>2440:R,A}\nqs{s>3448:A,lnx}\nqkq{x<1416:A,crn}\ncrn{x>2662:A,R}\nin{s<1351:px,qqz}\nqqz{s>2770:qs,m<1801:hdj,R}\ngd{a>3333:R,R}\nhdj{m>838:A,pv}\n\n{x=787,m=2655,a=1222,s=2876}\n{x=1679,m=44,a=2067,s=496}\n{x=2036,m=264,a=79,s=2244}\n{x=2461,m=1339,a=466,s=291}\n{x=2127,m=1623,a=2188,s=1013}"
-- 19114
solve (wfs, ps) = p1
  where
    p1 = sum $ sum <$> filter (isAccepted wfs) ps

main :: IO ()
main = readFile "input/day19.txt" >>= print . solve . parse
