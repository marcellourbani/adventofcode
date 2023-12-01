#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (foldr')
import Data.List (isPrefixOf, sort)

parse :: String -> [String]
parse = lines

calibrationValue :: String -> Int
calibrationValue s = read $ head digits : [last digits]
  where
    digits = filter (`elem` ['0' .. '9']) s

decodeDigits :: String -> String
decodeDigits = go
  where
    digits = [("one", "1"), ("two", "2"), ("three", "3"), ("four", "4"), ("five", "5"), ("six", "6"), ("seven", "7"), ("eight", "8"), ("nine", "9")]
    replaceDigit (pre, rep) txt
      | pre `isPrefixOf` txt = rep <> tail txt
      | otherwise = txt

    go txt
      | null txt = []
      | otherwise = head txt' : go (tail txt')
      where
        txt' = foldr' replaceDigit txt digits

-- >>> solve $ parse "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"
-- >>> sum $ calibrationValue . decodeDigits <$> parse "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"
-- (142,142)
-- 281
solve l = (p1, p2)
  where
    p1 = sum $ calibrationValue <$> l
    p2 = sum (calibrationValue . decodeDigits <$> l)

main :: IO ()
main = readFile "input/day01.txt" >>= print . solve . parse
