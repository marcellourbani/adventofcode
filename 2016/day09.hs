#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Either (fromRight)
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import qualified Data.Set as S
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (lookAhead, notFollowedBy), Parsec, anySingle, between, count, empty, runParser, some, try, (<|>))
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Segment = Simple String | Complex String Int deriving (Show)

type Parser = Parsec Void String

sc :: Parser () -- consume spaces
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parse s = fromRight [] $ runParser (some parseSegment) "" s

parseSegment :: Parser Segment
parseSegment = try parseComplex <|> try parseSimple
  where
    parseSimple = some alphaNumChar <&> Simple
    parseInt = some numberChar <&> read
    parseMarker = between (symbol "(") (symbol ")") $ (parseInt <&> (,)) <*> (symbol "x" *> parseInt)
    parseComplex = do
      (len, mul) <- parseMarker
      s <- count len anySingle
      return $ Complex s mul

segLength :: Segment -> Int
segLength a = case a of
  Simple s -> length s
  Complex s n -> n * length s

segLength2 :: Segment -> Int
segLength2 a = case a of
  Simple s -> length s
  Complex s n -> n * sum (segLength2 <$> parse s)

-- >>> solve $ parse "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"
-- (238,445)

solve :: [Segment] -> (Int, Int)
solve l = (sum $ segLength <$> l, sum $ segLength2 <$> l)

main :: IO ()
main = readFile "input/day09.txt" >>= print . solve . parse
