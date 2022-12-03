#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (foldl')
import Data.List.Split (chunksOf)
import qualified Data.Set as S

parse :: String -> [(String, String)]
parse s = sp <$> lines s
  where
    sp a = splitAt (div (length a) 2) a

priority :: Char -> Int
priority a
  | a >= 'a' = fromEnum a - fromEnum 'a' + 1
  | otherwise = fromEnum a - fromEnum 'A' + 27

-- >>> solve $ parse "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"
-- (157,70)
solve :: [(String, String)] -> (Int, Int)
solve l = (p1, p2)
  where
    p1 = sum $ priority <$> (l >>= shared)
    p2 = sum $ sum . map priority <$> badges
    shared (a, b) = S.toList $ S.intersection (S.fromList a) (S.fromList b)
    toSet (a, b) = S.fromList $ a <> b
    toBadge g = foldl' S.intersection (head g) g
    badges = S.toList . toBadge <$> chunksOf 3 (toSet <$> l)

main :: IO ()
main = readFile "input/day3.txt" >>= print . solve . parse
