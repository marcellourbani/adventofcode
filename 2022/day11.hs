#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Either (fromRight)
import Data.Functor (($>), (<&>))
import Data.List (partition, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Void (Void)
import Debug.Trace (trace)
import Text.Megaparsec (Parsec, empty, many, runParser, some, try, (<|>))
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char as LC
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser () -- consume spaces
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

type State = M.Map Int Monkey

symbol :: String -> Parser String
symbol = L.symbol sc

parse :: String -> [Monkey]
parse s = fromRight [] $ runParser (many monkey) "" s

data Operation = Mul Int | Add Int | Sq deriving (Show)

data Monkey = Monkey {mId :: Int, mItems :: [Int], mOperation :: Operation, mTest :: Int, mOnTrue :: Int, mOnfalse :: Int, mCount :: Int} deriving (Show)

signedInt :: Parser Int
signedInt = L.signed sc L.decimal

monkey :: Parser Monkey
monkey = lexeme $ Monkey <$> mId <*> items <*> lexeme operation <*> pTest <*> pOnTrue <*> pOnFalse <*> pure 0
  where
    mId = symbol "Monkey" *> L.decimal <* symbol ":" -- <* LC.newline
    items = sc *> symbol "Starting items:" *> some (try (lexeme L.decimal <* symbol ",") <|> lexeme L.decimal)
    pTest = lexeme $ symbol "Test: divisible by" *> L.decimal
    pOnTrue = lexeme $symbol "If true: throw to monkey" *> L.decimal
    pOnFalse = lexeme $ symbol "If false: throw to monkey" *> L.decimal

operation :: Parser Operation
operation = prefix *> (addp <|> try mulp <|> sqp)
  where
    addp = symbol "+" *> signedInt <&> Add
    mulp = symbol "*" *> signedInt <&> Mul
    sqp = symbol "* old" $> Sq
    prefix = symbol "Operation:" *> symbol "new" *> symbol "=" *> symbol "old"

runOp :: Operation -> Int -> Int
runOp o old = case o of
  Add v -> old + v
  Mul v -> old * v
  Sq -> old * old

turn :: (Operation -> Int -> Int) -> State -> State
turn conv state = go state $ M.keys state
  where
    go s l = case l of
      [] -> s
      i : is -> go s'' is
        where
          monkey@(Monkey _ items op tst destt destf cc) = s M.! i
          rect@(Monkey _ itt _ _ _ _ _) = s M.! destt
          recf@(Monkey _ itf _ _ _ _ _) = s M.! destf
          m' = monkey {mItems = [], mCount = cc + length items}
          worries = conv op <$> items
          (nitt, nitf) = partition ((== 0) . (`mod` tst)) worries
          s' = M.fromList [(i, m'), (destt, rect {mItems = itt <> nitt}), (destf, recf {mItems = itf <> nitf})]
          s'' = M.union s' s

-- >>> solve $ parse "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1"
-- (10605,2713310158)
solve :: [Monkey] -> (Int, Int)
solve l = (p counts1, p counts2)
  where
    initial = M.fromList $ zip (mId <$> l) l
    p c = product $ take 2 $ sortOn (\a -> - a) c
    conv1 o = (`div` 3) . runOp o
    conv2 o = (`mod` product (mTest <$> M.elems initial)) . runOp o
    counts1 = mCount <$> M.elems (iterate (turn conv1) initial !! 20)
    counts2 = mCount <$> M.elems (iterate (turn conv2) initial !! 10000)

main :: IO ()
main = readFile "input/day11.txt" >>= print . solve . parse
