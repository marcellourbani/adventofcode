#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

parse :: String -> [String]
parse = words

snafuDigitToInt :: Char -> Int
snafuDigitToInt c = case c of
  '=' -> -2
  '-' -> -1
  '0' -> 0
  '1' -> 1
  '2' -> 2
  _ -> 0

snafuToInt :: String -> Int
snafuToInt s = sum $ uncurry (*) <$> zip powers digits
  where
    digits = snafuDigitToInt <$> reverse s
    powers = (5 ^) <$> [0 ..]

intToSnafu :: Int -> String
intToSnafu i = go i ""
  where
    go n sn
      | n == 0 = sn
      | r < 3 = go d $ show r <> sn
      | r == 3 = go (d + 1) $ '=' : sn
      | otherwise = go (d + 1) $ '-' : sn
      where
        (d, r) = divMod n 5

-- >>> solve $ parse "1=-0-2\n 12111\n  2=0=\n    21\n  2=01\n   111\n 20012\n   112\n 1=-1=\n  1-12\n    12\n    1=\n   122"
-- "2=-1=0"

-- solve :: [String] -> String
solve l = p1
  where
    p1 = intToSnafu $ sum $ snafuToInt <$> l

main :: IO ()
main = readFile "input/day25.txt" >>= print . solve . parse
