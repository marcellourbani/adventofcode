#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Control.Monad (join)
import Data.Foldable (Foldable (foldl'))
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

newtype GameMap = GameMap {ungameMap :: M.Map Int (M.Map Int Int)}

instance Show GameMap where
  show m = unlines $ drawLine <$> [miny .. maxy]
    where
      (Point minx miny, Point maxx maxy) = mapLimits m
      drawValue v = if v == 0 then "." else show v
      drawLine y = join $ map drawValue $ gameMapPoint m <$> [minx .. maxx] <*> pure y

gameMapPoint :: GameMap -> Int -> Int -> Int
gameMapPoint (GameMap m) x y = M.findWithDefault 0 x $ M.findWithDefault M.empty y m

drawPoint :: GameMap -> Point -> GameMap
drawPoint (GameMap m) (Point x y) = GameMap newMap
  where
    oldRow = M.findWithDefault M.empty y m
    oldValue = M.findWithDefault 0 x oldRow
    newRow = M.insert x (oldValue + 1) oldRow
    newMap = M.insert y newRow m

drawLine :: GameMap -> Vector -> GameMap
drawLine m (Vector (Point x1 y1) (Point x2 y2))
  | x1 == x2 = foldl' drawPoint m ys
  | y1 == y2 = foldl' drawPoint m xs
  | otherwise = m
  where
    ys = Point x1 <$> [min y1 y2 .. max y1 y2]
    xs = Point <$> [min x1 x2 .. max x1 x2] <*> pure y1

drawDiagonals :: GameMap -> Vector -> GameMap
drawDiagonals m (Vector (Point x1 y1) (Point x2 y2))
  | x1 == x2 || y1 == y2 = m -- already drawn
  | maxx - minx == maxy - miny =
    foldl' drawPoint m $ uncurry Point <$> zip (r x1 x2) (r y1 y2)
  | otherwise = m
  where
    r a b = if a > b then [a, a -1 .. b] else [a .. b]
    maxx = max x1 x2
    minx = min x1 x2
    maxy = max y1 y2
    miny = min y1 y2
    x = gameMapPoint m <$> [minx .. maxx]

r a b = if a > b then [a, a -1 .. b] else [a .. b]

-- >>> fmap solveDebug $ parse [ "0,9 -> 5,9", "8,0 -> 0,8", "9,4 -> 3,4", "2,2 -> 2,1", "7,0 -> 7,4", "6,4 -> 2,0", "0,9 -> 2,9", "3,4 -> 1,4", "0,0 -> 8,8", "5,5 -> 8,2"]
-- Right (5,12,
-- 1.1....11.
-- .111...2..
-- ..2.1.111.
-- ...1.2.2..
-- .112313211
-- ...1.2....
-- ..1...1...
-- .1.....1..
-- 1.......1.
-- 222111....
-- )

mapLimits :: GameMap -> (Point, Point)
mapLimits (GameMap m) = (Point minx miny, Point maxx maxy)
  where
    rowkeys = M.keys m
    allcolkeys = foldl' (++) [] $ M.keys <$> m
    minx = minimum allcolkeys
    maxx = maximum allcolkeys
    miny = minimum rowkeys
    maxy = maximum rowkeys

solveDebug :: [Vector] -> (Int, Int, GameMap)
solveDebug m = (count, count2, endMap2)
  where
    endMap = foldl' drawLine (GameMap M.empty) m
    endMap2 = foldl' drawDiagonals endMap m
    count = sum $ M.size . M.filter (> 1) <$> ungameMap endMap
    count2 = sum $ M.size . M.filter (> 1) <$> ungameMap endMap2

solve :: [Vector] -> (Int, Int)
solve m = (count, count2) where (count, count2, _) = solveDebug m

parse :: [String] -> Either (ParseErrorBundle String Void) [Vector]
parse = traverse (runParser vectorParser "")

main :: IO ()
main = readFile "input/day5.txt" >>= print . fmap solve . parse . lines
