-- stack --resolver lts-18.18 script

module Main where

import Data.Functor
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Void
import Debug.Trace
import Text.Megaparsec (ErrorItem (EndOfInput), MonadParsec (try), Parsec, between, empty, failure, getOffset, getSourcePos, many, optional, runParser, setOffset, some, (<|>))
import Text.Megaparsec.Char

data Packet = Literal Int Int | Operator Int Int [Packet] deriving (Show, Eq)

type Parser = Parsec Void String

type Input = String

-- parse :: String -> Input
parse s = bits
  where
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

upTo :: Int -> Parser a -> Parser (Maybe a)
upTo l p = do
  iofs <- getOffset
  x <- try (p <&> Just) <|> return Nothing
  eofs <- getOffset
  if eofs - iofs <= l
    then return x
    else do
      setOffset iofs
      return Nothing

someUpTo :: Int -> Parser a -> Parser [a]
someUpTo l p = do
  iofs <- getOffset
  x <- try (upTo l p)
  eofs <- getOffset
  let foo = trace (show iofs <> ":" <> show eofs <> "-" <> show (l - (eofs - iofs)))
  case x of
    Nothing -> return []
    Just x' -> foo $ do
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
-- [Right (Literal 6 2021),Right (Operator 1 27 [Literal 6 10,Literal 2 20]),Right (Operator 7 3 [Literal 2 1,Literal 4 2,Literal 1 3])]

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
          return $ Operator vers len subs
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

-- solve :: Input -> (Int, Int)
solve i = i
  where

main :: IO ()
main = readFile "input/day16.txt" >>= print . solve . parse
