#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List (isInfixOf, sort)
import qualified Data.Set as S

parse :: String -> [String]
parse = lines

isNice :: String -> Bool
isNice s = hasvowels s && hasdouble s && not (blacklisted s)
  where
    hasvowels a = length (filter (`elem` "aeiou") a) >= 3
    hasdouble a = case a of
      (x : y : xs) | x == y -> True
      (x : xs) -> hasdouble xs
      _ -> False
    blacklisted a = case a of
      (x : y : xs) | S.member [x, y] blacklist -> True
      (x : xs) -> blacklisted xs
      _ -> False
    blacklist = S.fromList ["ab", "cd", "pq", "xy"]

isNice2 :: String -> Bool
isNice2 s = repeatpair s && hasRepeats s
  where
    repeatpair a = case a of
      (x : y : xs) | [x, y] `isInfixOf` xs -> True
      (x : xs) -> repeatpair xs
      _ -> False
    hasRepeats a = case a of
      (x : y : z : xs) | x == z && x /= y -> True
      (x : xs) -> hasRepeats xs
      _ -> False

-- >>> solve $ parse "ugknbfddgicrmopn\naaa\njchzalrnumimnmhp\nhaegwjzuvuyypxyu\ndvszwmarrgswjxmb\nqjhvhtzxzqqjkmpb\nxxyxx\nuurcxstgmygtbstg\nieodomkazucvgmuy"
-- (2,2)

-- solve :: [String] -> Int
solve l = (length $ filter isNice l, length $ filter isNice2 l)

main :: IO ()
main = readFile "input/day05.txt" >>= print . solve . parse
