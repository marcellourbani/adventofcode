#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Control.Arrow (first, second)
import Data.Either (fromRight)
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import qualified Data.Set as S
import Data.Void (Void)
import Text.Megaparsec (Parsec, empty, runParser, some, try, (<|>))
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Operation
  = Rect Int Int
  | RotateRow Int Int
  | RotateColumn Int Int
  deriving (Show)

data Grid = Grid
  { _width :: Int,
    _height :: Int,
    _grid :: S.Set (Int, Int)
  }

instance Show Grid where
  show (Grid width height m) = "width: " <> show width <> " height: " <> show height <> "\n" <> grid <> "-- "
    where
      grid = unlines $ drawLine <$> [0 .. height -1]
      drawLine y = "-- " <> (toChar <$> linePoints)
        where
          linePoints = zip [0 .. width -1] $ repeat y
          toChar p = if S.member p m then '#' else '.'

type Parser = Parsec Void String

sc :: Parser () -- consume spaces
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parse s = fromRight [] $ runParser (some parseOperation) "" s

parseOperation :: Parser Operation
parseOperation = try parseRect <|> try parseRotateRow <|> try parseRotateCol
  where
    parseRotateRow = symbol "rotate row" *> (RotateRow <$> (symbol "y=" *> parseInt) <*> (symbol "by" *> parseInt))
    parseRotateCol = symbol "rotate column" *> (RotateColumn <$> (symbol "x=" *> parseInt) <*> (symbol "by" *> parseInt))
    parseRect = symbol "rect" *> (Rect <$> parseInt <*> (symbol "x" *> parseInt))
    parseInt = lexeme $ some digitChar <&> read

applyOperation :: Grid -> Operation -> Grid
applyOperation g@(Grid width height s) o = g {_grid = s'}
  where
    s' = case o of
      Rect w h -> S.union s $ S.fromList $ (,) <$> [0 .. w -1] <*> [0 .. h -1]
      RotateRow y by -> S.union ot $ S.fromList $ first ((`mod` width) . (+ by)) <$> S.toList cr
        where
          (cr, ot) = S.partition ((== y) . snd) s
      RotateColumn x by -> S.union ot $ S.fromList $ second ((`mod` height) . (+ by)) <$> S.toList cr
        where
          (cr, ot) = S.partition ((== x) . fst) s

-- >>> solve $ parse "rect 3x2\nrotate column x=1 by 1\nrotate row y=0 by 4\nrotate column x=1 by 1"
-- (6,width: 50 height: 6
-- ....#.#...........................................
-- #.#...............................................
-- .#................................................
-- .#................................................
-- ..................................................
-- ..................................................
-- )

solve :: Foldable t => t Operation -> (Int, Grid)
solve l = (S.size g, p2)
  where
    initial = Grid 50 6 S.empty
    p2@(Grid _ _ g) = foldl' applyOperation initial l where initial = Grid 50 6 S.empty

main :: IO ()
main = readFile "input/day08.txt" >>= print . solve . parse
