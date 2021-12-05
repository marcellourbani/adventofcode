#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List (tails)

-- >>> solve [ ("forward", 5), ("down", 5), ("forward", 8), ("up", 3), ("down", 8), ("forward", 2) ]
-- (150,900)
solve :: [(String, Int)] -> (Int, Int)
solve l = (x * y, y' * x')
  where
    move (x, y) (action, n) = case action of
      "forward" -> (x + n, y)
      "down" -> (x, y + n)
      "up" -> (x, y - n)
      _ -> (x, y)
    moveWithAim (x, y, aim) (action, n) = case action of
      "forward" -> (x + n, n * aim + y, aim)
      "down" -> (x, y, aim + n)
      "up" -> (x, y, aim - n)
      _ -> (x, y, aim)
    (x, y) = foldl move (0, 0) l
    (x', y', _) = foldl moveWithAim (0, 0, 0) l

-- >>> parseLine "down 7"
-- ("down",7)
parseLine :: String -> (String, Int)
parseLine l = (head w, read $w !! 1)
  where
    w = words l

main :: IO ()
main = readFile "input/day2.txt" >>= print . solve . map parseLine . lines
