#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

module Main where

import Data.List (filter)
import Data.List.Split (splitOn)

data Equation = Equation {eqR :: Int, eqOp :: [Int]} deriving (Show)

type Input = [Equation]

parse :: String -> Input
parse s = pl <$> lines s
  where
    pl l = Equation (read p) (read <$> words e) where [p, e] = splitOn ": " l

combine :: [Int -> Int -> Int] -> [Int] -> [Int]
combine o l = case l of
  [] -> []
  [a] -> [a]
  a : b : xs -> (o <*> [a] <*> [b]) >>= combine o . (: xs)

joinInt :: Int -> Int -> Int
joinInt a b = read $ show a <> show b

valid :: [Int -> Int -> Int] -> Equation -> Bool
valid o (Equation r l) = r `elem` combine o l

part1 :: Input -> Int
part1 l = sum $ eqR <$> filter (valid [(+), (*)]) l

part2 :: Input -> Int
part2 l = sum $ eqR <$> filter (valid [(+), (*), joinInt]) l

-- >>> solve $ parse "190: 10 19\n3267: 81 40 27\n83: 17 5\n156: 15 6\n7290: 6 8 6 15\n161011: 16 10 13\n192: 17 8 14\n21037: 9 7 18 13\n292: 11 6 16 20"
-- (3749,11387)

solve :: Input -> (Int, Int)
solve l = (p1, p2)
  where
    p1 = part1 l
    p2 = part2 l

main :: IO ()
main = readFile "input/day07.txt" >>= print . solve . parse
