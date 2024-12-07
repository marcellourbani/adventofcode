#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

module Main where

import Text.Regex.TDFA

data Op = Mul Int Int | Do | Dont deriving (Show)

type Input = [Op]

instance Read Op where
  readsPrec :: Int -> ReadS Op
  readsPrec _ s = case s of
    'm' : 'u' : 'l' : rest -> [(Mul a b, "")] where (a, b) = read rest
    "don't()" -> [(Dont, "")]
    "do()" -> [(Do, "")]

parse :: String -> Input
parse s = lines s >>= readline
  where
    readline l = read <$> getAllTextMatches (l =~ "mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don't\\(\\)")

part1 :: Input -> Int
part1 l = case l of
  [] -> 0
  Mul a b : xs -> a * b + part1 xs
  _ : xs -> part1 xs

part2 :: Input -> Int
part2 = go True
  where
    go enabled l = case l of
      [] -> 0
      Do : xs -> go True xs
      Dont : xs -> go False xs
      Mul a b : xs
        | enabled -> a * b + go enabled xs
        | otherwise -> go enabled xs

-- >>> solve $

-- >>> solve$ parse "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
-- >>> solve$ parse "mul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
-- (161,161)
-- (161,48)

solve l = (p1, p2)
  where
    p1 = part1 l
    p2 = part2 l

main :: IO ()
main = readFile "input/day03.txt" >>= print . solve . parse
