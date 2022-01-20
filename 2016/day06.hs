#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List (sortBy, sortOn, transpose)
import qualified Data.Map.Strict as M

parse :: String -> [String]
parse = lines

-- >>> solve $ parse "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar"
-- ("easter","advent")

solve :: [String] -> (String, String)
solve l = (mostFreq <$> transpose l, leastFreq <$> transpose l)
  where
    leastFreq s = fst . head $ sortOn snd $ M.toList $ freq s
    mostFreq s = fst . last $ sortOn snd $ M.toList $ freq s
    freq s = M.unionsWith (+) $ M.singleton <$> s <*> [1]

main :: IO ()
main = readFile "input/day06.txt" >>= print . solve . parse
