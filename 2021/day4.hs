#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List (find, transpose)
import Data.List.Split (splitOn)
import Data.Set (Set, empty, insert, member)

sampleData :: String
sampleData =
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\
  \\n\
  \22 13 17 11  0\n\
  \ 8  2 23  4 24\n\
  \21  9 14 16  7\n\
  \ 6 10  3 18  5\n\
  \ 1 12 20 15 19\n\
  \\n\
  \ 3 15  0  2 22\n\
  \ 9 18 13 17  5\n\
  \19  8  7 25 23\n\
  \20 11 10 24  4\n\
  \14 21 16 12  6\n\
  \\n\
  \14 21 17 24  4\n\
  \10 16 15  9 19\n\
  \18  8 23 26 20\n\
  \22 11 13  6  5\n\
  \ 2  0 12  3  7\n"

type Card = [[Int]]

-- >>> parse sampleData
-- ([7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1],[[[22,13,17,11,0],[8,2,23,4,24],[21,9,14,16,7],[6,10,3,18,5],[1,12,20,15,19]],[[3,15,0,2,22],[9,18,13,17,5],[19,8,7,25,23],[20,11,10,24,4],[14,21,16,12,6]],[[14,21,17,24,4],[10,16,15,9,19],[18,8,23,26,20],[22,11,13,6,5],[2,0,12,3,7]]])
parse :: [Char] -> ([Int], [Card])
parse s = (draws, cards)
  where
    ls = lines s
    draws = read <$> splitOn "," (head ls)
    cards = parseCard <$> tail (splitOn "\n\n" s)
    parseCard = map (map read . words) . lines

-- >>> solve $ parse sampleData
-- (4512,1924)

solve :: ([Int], [[[Int]]]) -> (Int, Int)
solve (draws, cards) = (result * lastDraw, result2 * lastDraw2)
  where
    lineWins set line = and $ member <$> line <*> [set]
    (_, result, lastDraw, _) = bingo draws empty 0 cards
    (result2, lastDraw2, _) = ciapano draws cards []
    cardWins set card = go set card
      where
        go set card' = case card' of
          [] -> (False, 0)
          (line : lines) ->
            if lineWins set line
              then (True, sum [x | l <- card, x <- l, not $ member x set])
              else go set lines
    bingo d' set' last' cards' = go d' set' last'
      where
        go d set last = case (winner, winnerT) of
          (Just (idx, (True, n)), _) -> (True, n, last, idx)
          (_, Just (idx, (True, n))) -> (True, n, last, idx)
          _ -> case d of
            [] -> (False, 0, 0, 0)
            (d : ds) -> go ds (insert d set) d
          where
            cardsTransposed = transpose <$> cards'
            winner = find (fst . snd) $ zip [0 ..] (cardWins set <$> cards')
            winnerT = find (fst . snd) $ zip [0 ..] (cardWins set <$> cardsTransposed)
    ciapano d c acc = case bingo d empty 0 c of
      (True, tot, last, idx) -> ciapano d (remove idx c) ((tot, last, idx) : acc)
      _ -> head acc
      where
        remove i l = take i l ++ drop (i + 1) l

main :: IO ()
main = readFile "input/day4.txt" >>= print . solve . parse
