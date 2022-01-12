#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List (elemIndex, sort)

parse :: String -> [String]
parse = lines

-- >>> solve $ parse "\"\"\n\"abc\"\n\"aaa\\\"aaa\"\n\"\\x27\""
-- (12,19)

solve :: [String] -> (Int, Int)
solve l = (sum $dl <$> l, sum $dl2 <$> l)
  where
    hd = ['0' .. '9'] <> ['a' .. 'f']
    toch d1 d2 = fmap toEnum $ (+) <$> elemIndex d2 hd <*> ((16 *) <$> elemIndex d1 hd)

    dl s = length s - length (conv s)
    dl2 s = 2 + length (s >>= encodec) - length s
    encodec c = case c of
      '"' -> "\\\""
      '\\' -> "\\\\"
      _ -> [c]
    conv s = go $ take (length s - 2) $ tail s
    go s = case s of
      ('\\' : '\\' : xs) -> '\\' : go xs
      ('\\' : '\"' : xs) -> '\"' : go xs
      ('\\' : 'x' : a : b : xs) -> case toch a b of
        Just c -> c : go xs
        _ -> go $ tail s
      (x : xs) -> x : go xs
      "" -> ""

main :: IO ()
main = readFile "input/day08.txt" >>= print . solve . parse
