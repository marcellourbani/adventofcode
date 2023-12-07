#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import Data.Foldable (find)
import Data.List (elemIndex, partition, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

data HandType = HighCard | OnePair | TwoPair | Three | Full | Poker | Five deriving (Show, Eq, Ord)

data Hand = Hand HandType String deriving (Show, Eq)

data Hand' = Hand' HandType String deriving (Show, Eq)

instance Ord Hand where
  compare (Hand t c) (Hand t' c') = case compare t t' of
    EQ -> compare (cardStrength <$> c) (cardStrength <$> c')
    x -> x

instance Ord Hand' where
  compare (Hand' t c) (Hand' t' c') = case compare t t' of
    EQ -> compare (cardStrength' <$> c) (cardStrength' <$> c')
    x -> x

parse :: String -> [(Hand, Int)]
parse s = parseLine . words <$> lines s where parseLine [a, b] = (Hand (handType a) a, read b)

cardStrength :: Char -> Int
cardStrength c = 12 - fromMaybe 0 (elemIndex c "AKQJT98765432")

cardStrength' :: Char -> Int
cardStrength' c = 12 - fromMaybe 0 (elemIndex c "AKQT98765432J")

handType :: String -> HandType
handType h
  | M.size frequencies == 1 = Five
  | find (== 4) frequencies == Just 4 = Poker
  | hasThree && (M.size frequencies == 2) = Full
  | hasThree = Three
  | M.size pairs == 2 = TwoPair
  | M.size pairs == 1 = OnePair
  | otherwise = HighCard
  where
    frequencies = M.unionsWith (+) $ M.singleton <$> h <*> [1]
    hasThree = find (== 3) frequencies == Just 3
    pairs = M.filter (== 2) frequencies

handType' :: [Char] -> HandType
handType' h = case (length wilds, handType h) of
  (4, _) -> Five
  (3, Three) -> Poker
  (3, Full) -> Five
  (2, Full) -> Five
  (2, OnePair) -> Three
  (2, TwoPair) -> Poker
  (1, Poker) -> Five
  (1, Three) -> Poker
  (1, TwoPair) -> Full
  (1, OnePair) -> Three
  (1, HighCard) -> OnePair
  (_, t) -> t
  where
    (wilds, others) = partition (== 'J') h

convert :: Hand -> Hand'
convert (Hand _ c) = Hand' (handType' c) c

-- >>> solve $ parse "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"
-- (6440,5905,[(Hand' OnePair "32T3K",765),(Hand' TwoPair "KK677",28),(Hand' Poker "T55J5",684),(Hand' Poker "QQQJA",483),(Hand' Poker "KTJJT",220)])
solve :: [(Hand, Int)] -> (Int, Int)
solve l = (p1, p2)
  where
    ranked = zip [1 ..] $ snd <$> sort l
    converted = zip (convert . fst <$> l) $ snd <$> l
    ranked' = zip [1 ..] $ snd <$> sort converted
    p1 = sum $ uncurry (*) <$> ranked
    p2 = sum $ uncurry (*) <$> ranked'

main :: IO ()
main = readFile "input/day07.txt" >>= print . solve . parse
