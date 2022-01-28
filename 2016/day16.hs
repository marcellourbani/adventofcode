#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

parse s = s

-- >>> solve $ parse "111100001010"
-- "5 10 25"

next :: String -> String
next a = a <> "0" <> (i <$> reverse a) where i c = if c == '1' then '0' else '1'

fill :: String -> Int -> String
fill a n
  | length a < n = fill (next a) n
  | otherwise = take n a

checksum :: String -> String
checksum s = if odd $length cs then cs else checksum cs
  where
    cs = go s
    go (a : b : xs)
      | a == b = '1' : go xs
      | otherwise = '0' : go xs
    go _ = []

-- >>> checksum $ fill "10000" 20
-- "01100"

solve :: String -> (String, String)
solve l = (checksum $ fill l 272, checksum $ fill l 35651584)

main :: IO ()
main = readFile "input/day16.txt" >>= print . solve . parse
