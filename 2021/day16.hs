#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Either (fromRight)
import Data.Functor
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Void
import Text.Megaparsec (ErrorItem (EndOfInput), Parsec, between, empty, failure, getOffset, getSourcePos, many, optional, parseTest, runParser, setOffset, some, try, (<|>))
import Text.Megaparsec.Char

data Packet = Literal Int Int | Operator Int Int [Packet] deriving (Show, Eq)

type Parser = Parsec Void String

type Input = Packet

parse :: String -> Packet
parse s = fromRight (Literal 0 0) parsed
  where
    parsed = runParser parsePacket "" bits
    bits = s >>= hexBin . hexChar
    hexChar c = fromMaybe 0 $ elemIndex c "0123456789ABCDEF"
    hexBin i = go i ""
      where
        go 0 s = replicate (4 - length s) '0' ++ s
        go i s = go (div i 2) $ show (mod i 2) ++ s

ignoreTrailing :: Parser a -> Parser a
ignoreTrailing p = do
  x <- p
  ofs <- getOffset
  let slack = 4 - mod ofs 4
  _ <- times slack $ char '0'
  return x

upTo :: Int -> Parser a -> Parser a
upTo l p = do
  iofs <- getOffset
  x <- p
  eofs <- getOffset
  if eofs - iofs <= l
    then return x
    else do
      -- setOffset iofs
      failure (Just EndOfInput) S.empty

someUpTo :: Int -> Parser a -> Parser [a]
someUpTo l p = do
  iofs <- getOffset
  x <- optional . try $ upTo l p
  eofs <- getOffset
  case x of
    Nothing -> return []
    Just x' -> do
      next <- someUpTo (l - (eofs - iofs)) p
      pure (x' : next)

parseValue :: Parser Int
parseValue = go <&> tot
  where
    parseLast = (char '0' *> parseBits 4) <&> (: [])
    parseNotLast = (char '1' *> parseBits 4) <&> (: [])
    go = parseLast <|> (parseNotLast <> go)
    tot l = sum [x * e | (x, e) <- zip (reverse l) (iterate (* 16) 1)]

-- >>> runParser parsePacket "" <$> ["110100101111111000101000","00111000000000000110111101000101001010010001001000000000","11101110000000001101010000001100100000100011000001100000"]
-- [Right (Literal 6 2021),Right (Operator 1 6 [Literal 6 10,Literal 2 20]),Right (Operator 7 3 [Literal 2 1,Literal 4 2,Literal 1 3])]

parsePacket :: Parser Packet
parsePacket = ignoreTrailing go
  where
    go = do
      vers <- parseBits 3
      ptype <- parseBits 3
      if ptype == 4
        then Literal vers <$> parseValue
        else do
          lenType <- parseBit
          len <- parseLen lenType
          subs <- subPackets len lenType
          return $ Operator vers ptype subs
    parseLen lenType = do
      if lenType == 0
        then parseBits 15
        else parseBits 11
    subPackets len lt = case lt of
      1 -> times len go
      _ -> someUpTo len go

parseBit :: Parser Int
parseBit = digitChar <&> read . (: [])

parseBits :: Int -> Parser Int
parseBits 0 = pure 0
parseBits n = do
  cb <- parseBit <&> (* 2 ^ (n -1))
  rest <- parseBits (n -1)
  return $ cb + rest

parseBitString :: Int -> Parser String
parseBitString 0 = pure ""
parseBitString n = (digitChar <&> (: [])) <> parseBitString (n - 1)

times :: Int -> Parser a -> Parser [a]
times 0 _ = pure []
times n p = (:) <$> p <*> times (n - 1) p

eval :: Packet -> Int
eval (Literal _ v) = v
eval (Operator _ o subs) = case o of
  0 -> sum ops
  1 -> product ops
  2 -> minimum ops
  3 -> maximum ops
  5 -> case ops of
    [x, y] | x > y -> 1
    _ -> 0
  6 -> case ops of
    [x, y] | x < y -> 1
    _ -> 0
  7 -> case ops of
    [x, y] | x == y -> 1
    _ -> 0
  _ -> 0
  where
    ops = eval <$> subs

-- >>> solve .parse $ "9C0141080250320F1802104A08"
-- (20,1)

solve :: Input -> (Int, Int)
solve i = (totv, val)
  where
    totv = sumVersions i
    val = eval i
    sumVersions pk = case pk of
      Literal v _ -> v
      Operator v _ sp -> v + sum (sumVersions <$> sp)
    foo = parsePacket
    foo2 = do
      x <- parsePacket
      o <- getOffset
      return (x, o)

main :: IO ()
main = readFile "input/day16.txt" >>= print . solve . parse
