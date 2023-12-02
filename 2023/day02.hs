#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

data Color = Red | Green | Blue deriving (Show, Eq, Ord)

data Game = Game {gameId :: Int, rounds :: [[(Color, Int)]]} deriving (Show, Eq)

-- parse :: String -> [String]
parse s = parseGame <$> lines s
  where
    parseItem i = case words i of
      [n, "red"] -> (Red, read n)
      [n, "green"] -> (Green, read n)
      [n, "blue"] -> (Blue, read n)
    parseRound r = parseItem <$> splitOn "," r
    parseGame l = Game (read n) (parseRound <$> rounds)
      where
        [h, t] = splitOn ":" l
        rounds = splitOn ";" t
        [_, n] = words h

possibleRound :: [(Color, Int)] -> Bool
possibleRound l = case l of
  [] -> True
  (Red, n) : cs -> n <= 12 && possibleRound cs
  (Green, n) : cs -> n <= 13 && possibleRound cs
  (Blue, n) : cs -> n <= 14 && possibleRound cs

possibleGame :: Game -> Bool
possibleGame g = and $ possibleRound <$> rounds g

power g = product needed
  where
    needed = M.unionsWith max $ M.fromList <$> rounds g

-- >>> solve $ parse "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
-- (8,2286)
solve l = (p1, p2)
  where
    p1 = sum $ gameId <$> filter possibleGame l
    p2 = sum $ power <$> l

main :: IO ()
main = readFile "input/day02.txt" >>= print . solve . parse
