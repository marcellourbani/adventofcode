#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import Data.Either (fromRight)
import Data.Functor (($>))
import Data.List (elemIndex, find, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Ratio (denominator, numerator, (%))
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

data Op = Mul | Div | Add | Sub deriving (Show, Eq)

data Monkey = Imm String Int | Comp String String Op String deriving (Show, Eq)

data MonkeyNumber = Human | Mnum Rational | MComp Op MonkeyNumber MonkeyNumber deriving (Show)

-- simplify :: MonkeyNumber -> MonkeyNumber
-- simplify n = case n of
--   Human -> n
--   Mnum _ -> n
--   MComp _ Human _ -> n
--   MComp _ _ Human -> n
--   MComp Add
a = MComp Mul (Mnum (4400 % 189)) Human

b = Mnum 4

c = Mnum 3

d = b + a

e = c + d

-- >>> a + b + c
-- >>> b + c + a
-- >>> b +a+ c
-- MComp Add (Mnum (7 % 1)) (MComp Mul (Mnum (4400 % 189)) Human)
-- MComp Add (Mnum (7 % 1)) (MComp Mul (Mnum (4400 % 189)) Human)
-- MComp Add (Mnum (7 % 1)) (MComp Mul (Mnum (4400 % 189)) Human)
instance Num MonkeyNumber where
  a + b = case (a, b) of
    (Mnum ai, Mnum bi) -> Mnum $ ai + bi
    (MComp {}, Mnum _) -> b + a
    (Mnum 0, _) -> b
    (Mnum _, MComp Add bi@(Mnum _) y) -> MComp Add (a + bi) y
    (Mnum _, MComp Add y bi@(Mnum _)) -> MComp Add y (a + bi)
    (Mnum _, MComp Sub y bi@(Mnum _)) -> MComp Add y (a - bi)
    (Mnum _, MComp Sub bi@(Mnum _) y) -> MComp Sub (a + bi) y
    (_, _) -> MComp Add a b
  a - b = case (a, b) of
    (Mnum ai, Mnum bi) -> Mnum $ ai - bi
    (_, Mnum 0) -> a
    (Mnum _, MComp Add bi@(Mnum _) y) -> MComp Sub (a - bi) y
    (Mnum _, MComp Add y bi@(Mnum _)) -> MComp Sub (a - bi) y
    (Mnum _, MComp Sub y bi@(Mnum _)) -> MComp Sub (a + bi) y
    (Mnum _, MComp Sub bi@(Mnum _) y) -> MComp Add (a - bi) y
    (MComp Add bi@(Mnum _) y, Mnum _) -> MComp Sub y (b - bi)
    (MComp Add y bi@(Mnum _), Mnum _) -> MComp Sub y (b - bi)
    (MComp Sub y bi@(Mnum _), Mnum _) -> MComp Sub y (bi + b)
    (MComp Sub bi@(Mnum _) y, Mnum _) -> MComp Sub (bi - b) y
    (_, _) -> MComp Sub a b
  a * b = case (a, b) of
    (Mnum ai, Mnum bi) -> Mnum $ ai * bi
    (MComp {}, Mnum _) -> b * a
    (Mnum 0, _) -> a
    (_, Mnum 0) -> b
    (Mnum m, MComp Add x y) -> MComp Add (a * x) (a * y)
    (Mnum m, MComp Sub x y) -> MComp Sub (a * x) (a * y)
    (Mnum m, MComp Mul (Mnum x) y) -> MComp Mul (Mnum $ m * x) y
    (Mnum m, MComp Mul y (Mnum x)) -> MComp Mul (Mnum $ m * x) y
    (Mnum m, MComp Div (Mnum x) y) -> MComp Mul (Mnum $ m / x) y
    (Mnum m, MComp Div y (Mnum x)) -> MComp Mul y (Mnum $ m / x)
    (_, _) -> MComp Mul a b
  abs = undefined
  signum = undefined
  fromInteger i = Mnum $ fromInteger i

class Num a => Divable a where
  mydiv :: a -> a -> a

instance Divable Int where
  mydiv = div

instance Divable MonkeyNumber where
  mydiv a b = case (a, b) of
    (_, Mnum 1) -> a
    (Mnum ai, Mnum bi) -> Mnum $ ai / bi
    (MComp Add x y, Mnum d) -> MComp Add (mydiv x b) (mydiv y b)
    (MComp Sub x y, Mnum d) -> MComp Sub (mydiv x b) (mydiv y b)
    (MComp Mul (Mnum x) y, Mnum d) -> MComp Mul (Mnum $ x / d) y
    (MComp Mul y (Mnum x), Mnum d) -> MComp Mul (Mnum $ x / d) y
    (MComp Div (Mnum x) y, Mnum d) -> MComp Div (Mnum $ x / d) y
    (MComp Div y (Mnum x), Mnum d) -> MComp Div y (Mnum $ x * d)
    (_, _) -> MComp Div a b

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

monkeyName :: Monkey -> String
monkeyName m = case m of
  Imm n _ -> n
  Comp n _ _ _ -> n

calc :: Divable a => Op -> a -> a -> a
calc op o1 o2 = case op of
  Mul -> o1 * o2
  Div -> mydiv o1 o2
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

elaborate :: [Monkey] -> M.Map String MonkeyNumber -> ([Monkey], M.Map String MonkeyNumber)
elaborate monkeys known = go monkeys known []
  where
    go monkeys' known rest' = case monkeys' of
      [] -> (reverse rest', known)
      m@(Imm name v) : ms
        | name == "humn" -> go ms (M.insert name Human known) rest'
        | otherwise -> go ms (M.insert name (Mnum $ fromIntegral v) known) rest'
      m@(Comp name o1 op o2) : ms -> case (known M.!? o1, known M.!? o2) of
        (Just o1n, Just o2n) -> go ms (M.insert name (calc op o1n o2n) known) rest'
        _ -> go ms known $ m : rest'

elaborate2 :: [Monkey] -> M.Map String MonkeyNumber -> ([Monkey], M.Map String MonkeyNumber)
elaborate2 monkeys known
  | monkeys' == monkeys = (monkeys, known)
  | otherwise = elaborate2 monkeys' known'
  where
    (monkeys', known') = elaborate monkeys known

normalize :: MonkeyNumber -> MonkeyNumber
normalize n = case n of
  MComp Sub a (Mnum b) -> MComp Add a (Mnum $ -b)
  MComp Div a (Mnum b) -> MComp Mul a (Mnum $ 1 / b)
  MComp Add a@(Mnum _) b@(MComp {}) -> normalize $ MComp Add b a
  MComp Mul a@(Mnum _) b@(MComp {}) -> normalize $ MComp Mul b a
  _ -> n

part2 :: [Monkey] -> Int
part2 ms = fromInteger $ div (numerator solution) (denominator solution)
  where
    ops = filter (\m -> monkeyName m /= "root") ms -- && monkeyName m /= "humn") ms
    (remainder, known) = elaborate2 ops M.empty

    (goal1key, goal2key) = case find (\m -> monkeyName m == "root") ms of
      Just (Comp _ p1 _ p2) -> (p1, p2)
      _ -> undefined
    goal1 = known M.! goal1key
    goal2 = known M.! goal2key
    equation = goal1 - goal2
    solution = case normalize equation of
      MComp Sub (MComp Mul (Mnum a) Human) (Mnum b) -> b / a
      MComp Sub (Mnum b) (MComp Mul (Mnum a) Human) -> b / a
      MComp Add (MComp Mul (Mnum a) Human) (Mnum b) -> -b / a
      _ -> undefined

-- >>> solve $ parse "root: pppw + sjmn\ndbpl: 5\ncczh: sllz + lgvd\nzczc: 2\nptdq: humn - dvpt\ndvpt: 3\nlfqf: 4\nhumn: 5\nljgn: 2\nsjmn: drzm * dbpl\nsllz: 4\npppw: cczh / lfqf\nlgvd: ljgn * ptdq\ndrzm: hmdt - zczc\nhmdt: 32"
-- 301
solve :: [Monkey] -> (Int, Int)
solve l = (part1 l, part2 l)

main :: IO ()
main = readFile "input/day21.txt" >>= print . solve . parse
