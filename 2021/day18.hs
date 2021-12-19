#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Either (rights)
import Data.Foldable (Foldable (foldr'), foldl')
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, empty, runParser, some, (<|>))
import Text.Megaparsec.Char

data BaseNum = SN Int | CN FishNum
  deriving (Eq, Ord)

data FishNum = FN BaseNum BaseNum deriving (Eq, Ord)

instance Show FishNum where
  show (FN a b) = "[" ++ show a ++ "," ++ show b ++ "]"

instance Show BaseNum where
  show (SN n) = show n
  show (CN n) = show n

type Input = [FishNum]

type Parser = Parsec Void String

parse :: String -> Input
parse s = rights $ runParser parseFisfNum "" <$> lines s

parseInt :: Parser BaseNum
parseInt = some digitChar <&> (SN . read)

parseFisfNum :: Parser FishNum
parseFisfNum = between (char '[') (char ']') $ do
  a <- parseInt <|> parseSub
  _ <- char ','
  b <- parseInt <|> parseSub
  pure $ FN a b
  where
    parseSub = parseFisfNum <&> CN

addNums :: FishNum -> FishNum -> FishNum
addNums a b = reduce $ FN (CN a) (CN b)

sumNums :: [FishNum] -> FishNum
sumNums l = foldl' addNums (head l) (tail l)

serialize :: FishNum -> String
serialize (FN a b) = "FN (" <> serBN a <> ") (" <> serBN b <> ")"
  where
    serBN (SN n) = "SN " <> show n
    serBN (CN n) = "CN (" <> serialize n <> ")"

reduce :: FishNum -> FishNum
reduce n = case (n == exploded, n == splitted) of
  (False, _) -> reduce exploded
  (_, False) -> reduce splitted
  _ -> n
  where
    exploded = explode n
    splitted = splitN n

splitN :: FishNum -> FishNum
splitN (FN a b) = FN a' b'
  where
    a' = splitb a
    b' = if a == a' then splitb b else b
    splitb (SN x)
      | x > 9 = CN $ FN (SN x') (SN (x - x'))
      | otherwise = SN x
      where
        x' = floor $fromIntegral x / 2
    splitb (CN (FN x y)) = CN (FN x' y')
      where
        x' = splitb x
        y' = if x == x' then splitb y else y

explode :: FishNum -> FishNum
explode nu = fromMaybe nu $ go nu 0 []
  where
    rebuild s = case foldr' addStackEl (SN 0) (reverse s) of
      CN a -> a
      _ -> error "rebuild"
    addStackEl s n = case s of
      Left a -> CN $ FN a n
      Right a -> CN $ FN n a
    addToLastR bn v = case bn of
      (SN a) -> SN (a + v)
      CN (FN a b) -> CN (FN a $ addToLastR b v)

    addToLastL bn v = case bn of
      (SN a) -> SN (a + v)
      CN (FN a b) -> CN (FN (addToLastL a v) b)

    addback l (a, b) = case l of
      [] -> []
      Left x : xs -> Left (addToLastR x a) : addback xs (0, b)
      Right y : xs -> Right (addToLastL y b) : addback xs (a, 0)
    simpleNum n = case n of
      (FN (SN a) (SN b)) -> Just (a, b)
      _ -> Nothing
    go num depth st = case (depth, num) of
      (n, nu) | n >= 4 -> case simpleNum nu of
        Nothing -> Nothing
        Just a -> Just $ rebuild (addback st a)
      (n, FN a b) -> case (a, b) of
        (CN a, CN b) -> go a (n + 1) (Right (CN b) : st) <|> go b (n + 1) (Left (CN a) : st)
        (CN a, b) -> go a (n + 1) (Right b : st)
        (a, CN b) -> go b (n + 1) (Left a : st)
        _ -> Nothing

magnitude :: FishNum -> Int
magnitude (FN a b) = 3 * go a + 2 * go b
  where
    go (SN n) = n
    go (CN n) = magnitude n

-- >>> solve  $parse "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]\n[[[5,[2,8]],4],[5,[[9,9],0]]]\n[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]\n[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]\n[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]\n[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]\n[[[[5,4],[7,7]],8],[[8,3],8]]\n[[9,3],[[9,9],[6,[4,9]]]]\n[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]\n[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
-- (4140,3993)

solve :: Input -> (Int, Int)
solve i = (p1, p2)
  where
    p1 = magnitude $ sumNums i
    p2 = maximum [magnitude $addNums a b | a <- i, b <- i]

main :: IO ()
main = readFile "input/day18.txt" >>= print . solve . parse
