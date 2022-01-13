#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import qualified Data.Set as S

type Password = [Int]

parse :: String -> Password
parse i = reverse $ toInt <$> i

toInt :: Char -> Int
toInt c = fromEnum c - fromEnum 'a'

blacklist :: S.Set Int
blacklist = S.fromList $ toInt <$> "iol"

upper :: Int
upper = toInt 'z'

add1 :: Password -> Password
add1 n = case n of
  [] -> []
  (x : xs) | x == upper -> 0 : add1 xs
  (x : xs) -> (x + 1) : xs

valid :: Password -> Bool
valid p = notbl && hasseq p && hasDouble p Nothing
  where
    notbl = all (`S.notMember` blacklist) p
    hasseq l = case l of
      a : b : c : xs | b == a -1 && c == a -2 -> True
      x : xs -> hasseq xs
      _ -> False
    hasDouble l c = case (l, c) of
      (a : b : xs, Just d) | a == b && d /= a -> True
      (a : b : xs, Nothing) | a == b -> hasDouble xs $ Just a
      (x : xs, _) -> hasDouble xs c
      _ -> False

toStr :: Password -> String
toStr p = toEnum . (+ fromEnum 'a') <$> reverse p

noBl :: Password -> Password
noBl p = go [] $ reverse p
  where
    go l p = case p of
      [] -> l
      (x : xs) | S.member x blacklist -> replicate (length xs) 0 ++ (x + 1 : l)
      (x : xs) -> go (x : l) xs

next :: Password -> Password
next p = noBl $ add1 p

-- >>> solve $ parse "abcdefgh"
-- >>> solve $ parse "ghjaabcc"
-- ("abcdffaa","abcdffbb")
-- ("ghjbbcdd","ghjccdee")

solve l = (toStr p1, toStr p2)
  where
    p1 = go $ next l
    p2 = go $ next p1
    go p = if valid p then p else go $ next p

main :: IO ()
main = readFile "input/day11.txt" >>= print . solve . parse
