#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Either (fromRight)
import Data.Functor (($>))
import Data.List (elemIndex, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as S
import Data.Void (Void)
import Debug.Trace (trace)
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

data Op = Mul | Div | Add | Sub deriving (Show)

data Monkey = Imm String Int | Comp String String Op String deriving (Show)

parseOp :: Parser Op
parseOp = lexeme $ symbol "*" $> Mul <|> symbol "/" $> Div <|> symbol "+" $> Add <|> symbol "-" $> Sub

parseId :: Parser String
parseId = lexeme $ many LC.alphaNumChar

parseMonkey :: Parser Monkey
parseMonkey =
  try (Comp <$> parseId <* lexeme (LC.char ':') <*> parseId <*> parseOp <*> parseId)
    <|> Imm <$> parseId <* lexeme (LC.char ':') <*> lexeme L.decimal

parseInput :: Parser [Monkey]
parseInput = many parseMonkey

parse :: String -> [Monkey]
parse s = fromRight [] $ runParser parseInput "" s

calc :: Op -> Int -> Int -> Int
calc op o1 o2 = case op of
  Mul -> o1 * o2
  Div -> div o1 o2
  Add -> o1 + o2
  Sub -> o1 - o2

part1 :: [Monkey] -> Int
part1 ml = go known missing
  where
    val (Imm mid v) = M.singleton mid v
    val _ = M.empty
    known = M.unions $ val <$> ml
    mis (Comp mid o1 op o2) = M.singleton mid (op, o1, o2)
    mis _ = M.empty
    missing = M.unions $ mis <$> ml
    go kn mi
      | M.member "root" kn = kn M.! "root"
      | M.null toadd = trace (show kn) 0
      | otherwise = go kn' mi'
      where
        comp (op, o1, o2) = calc op <$> M.lookup o1 kn <*> M.lookup o2 kn
        toadd = fromMaybe 0 <$> M.filter isJust (comp <$> mi)
        kn' = M.union toadd kn
        mi' = M.difference mi toadd

-- >>> solve $ parse "root: pppw + sjmn\ndbpl: 5\ncczh: sllz + lgvd\nzczc: 2\nptdq: humn - dvpt\ndvpt: 3\nlfqf: 4\nhumn: 5\nljgn: 2\nsjmn: drzm * dbpl\nsllz: 4\npppw: cczh / lfqf\nlgvd: ljgn * ptdq\ndrzm: hmdt - zczc\nhmdt: 32"
-- 152
solve l = part1 l

main :: IO ()
main = readFile "input/day21.txt" >>= print . solve . parse
