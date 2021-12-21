#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

data Board = Board {p1 :: Int, p2 :: Int, dice :: Int} deriving (Show, Eq)

type Input = Board

parse :: String -> Input
parse s = Board a b 0
  where
    [a, b] = read . (: "") . last <$> lines s

turn :: Board -> Bool -> Board
turn (Board pl1 pl2 d) p1Turn = Board pl1' pl2' d'
  where
    throw (cd, s) = (mod (cd + 1) 100, mod (s + cd) 10 + 1)
    is = if p1Turn then pl1 else pl2
    (d', score) = iterate throw (d, is) !! 3
    (pl1', pl2') = if p1Turn then (score, pl2) else (pl1, score)

game :: Board -> (Board, Int, Int)
game b = go b True 0 0 0
  where
    go (Board pl1 pl2 d) p1t sc1 sc2 rolls =
      case (sc1 >= 1000, sc2 >= 1000) of
        (True, _) -> (b, sc2, rolls)
        (_, True) -> (b, sc1, rolls)
        _ -> go b' (not p1t) sc1' sc2' (rolls + 3)
      where
        b' = turn (Board pl1 pl2 d) p1t
        sc1' = if p1t then sc1 + p1 b' else sc1
        sc2' = if p1t then sc2 else sc2 + p2 b'

-- >>> solve  $parse "Player 1 starting position: 4\nPlayer 2 starting position: 8"
-- 739785

solve :: Board -> Int
solve i = other * rolls
  where
    (_, other, rolls) = game i

main :: IO ()
main = readFile "input/day21.txt" >>= print . solve . parse
