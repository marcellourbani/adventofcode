#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import qualified Data.Map.Strict as M
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

type GameMap = M.Map Int (M.Map Int Int)

gameMapPoint :: GameMap -> Int -> Int -> Int
gameMapPoint m x y = M.findWithDefault 0 x $ M.findWithDefault M.empty y m

drawPoint :: GameMap -> Point -> GameMap
drawPoint m (Point x y) = newMap
  where
    oldRow = M.findWithDefault M.empty y m
    oldValue = M.findWithDefault 0 x oldRow
    newRow = M.insert x (oldValue + 1) oldRow
    newMap = M.insert y newRow m

drawLine :: GameMap -> Vector -> GameMap
drawLine m (Vector (Point x1 y1) (Point x2 y2))
  | x1 == x2 = foldl drawPoint m ys
  | y1 == y2 = foldl drawPoint m xs
  | otherwise = m
  where
    ys = Point x1 <$> [min y1 y2 .. max y1 y2]
    xs = Point <$> [min x1 x2 .. max x1 x2] <*> pure y1

-- >>> fmap solve $ parse [ "0,9 -> 5,9", "8,0 -> 0,8", "9,4 -> 3,4", "2,2 -> 2,1", "7,0 -> 7,4", "6,4 -> 2,0", "0,9 -> 2,9", "3,4 -> 1,4", "0,0 -> 8,8", "5,5 -> 8,2" ]
-- Right (5,0)
solve :: [Vector] -> (Int, Int)
solve m = (count, 0)
  where
    endMap = foldl drawLine M.empty m
    count = sum $ M.size . M.filter (> 1) <$> endMap

-- >>> parse "0,9 -> 5,9"
-- 2
parse :: [String] -> Either (ParseErrorBundle String Void) [Vector]
parse = traverse (runParser vectorParser "")

main :: IO ()
main = readFile "input/day5.txt" >>= print . fmap solve . parse . lines
