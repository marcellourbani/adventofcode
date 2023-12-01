#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Control.Arrow (Arrow (second))
import Data.List (sort)
import Data.List.Split (splitOn)

data Hand = A | B | C deriving (Show, Read, Enum, Eq)

data SecretHand = X | Y | Z deriving (Show, Read, Enum, Eq)

type Round = (Hand, SecretHand)

type Strategy = SecretHand -> Hand

parse :: String -> [Round]
parse s = readItem <$> items
  where
    readItem [h, s] = (read h, read s)
    readItem _ = undefined
    items = words <$> lines s

baseStrategy :: Strategy
baseStrategy = toEnum . fromEnum

playRound :: (Hand, Hand) -> Int
playRound (other, me) = mine + 1 + game
  where
    mine = fromEnum me
    game
      | other == me = 3
      | mine - fromEnum other == 1 = 6
      | me == A && other == C = 6
      | otherwise = 0

strategyRound :: Strategy -> Round -> Int
strategyRound strategy r = playRound $ second strategy r

playByActualStrategy :: Round -> Int
playByActualStrategy (elf, me) = game + 1 + fromEnum myPlay
  where
    game = 3 * fromEnum me
    elfnum = fromEnum elf
    myPlay = case me of
      Y -> elf
      X -> toEnum $ mod (elfnum - 1) 3
      Z -> toEnum $ mod (elfnum + 1) 3

-- >>> solve $ parse "A Y\nB X\nC Z"
-- (15,12)

solve :: [Round] -> (Int, Int)
solve l = (p1, p2)
  where
    p1 = sum $ strategyRound baseStrategy <$> l
    p2 = sum $ playByActualStrategy <$> l

main :: IO ()
main = readFile "input/day02.txt" >>= print . solve . parse
