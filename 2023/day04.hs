#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Card = Card {cardId :: Int, cardWin :: [Int], cardHas :: [Int]} deriving (Show)

parse :: String -> [Card]
parse s = parseline <$> lines s
  where
    parseline l = Card cardNumber w he
      where
        [h, rest] = splitOn ":" l
        cardNumber = read $ words h !! 1
        [w, he] = map read . words <$> splitOn "|" rest

matchNumber :: Card -> Int
matchNumber (Card _ w h) = S.size $ S.intersection (S.fromList w) (S.fromList h)

cardScore :: Card -> Int
cardScore c = case matchNumber c of
  0 -> 0
  n -> 2 ^ (n - 1)

winCards :: [Card] -> M.Map Int Int
winCards cards = go cards initial
  where
    initial = M.fromList $ (,1) <$> (cardId <$> cards)
    go l m = case l of
      [] -> m
      c@(Card cardid _ _) : cs -> case matchNumber c of
        0 -> go cs m
        wins -> go cs $ M.unionWith (+) m' m
          where
            winningCards = M.findWithDefault 1 cardid m
            m' = M.fromList $ (,winningCards) <$> [cardid + 1 .. cardid + wins] -- assumes sequential IDs

-- >>> solve $ parse "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
-- (13,30)
solve :: [Card] -> (Int, Int)
solve l = (p1, p2)
  where
    p1 = sum $ cardScore <$> l
    p2 = sum $ winCards l

main :: IO ()
main = readFile "input/day04.txt" >>= print . solve . parse
