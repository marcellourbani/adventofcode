#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

parse :: String -> [[String]]
parse s = words <$> lines s

-- >>> solve $ parse "aa bb cc dd ee\naa bb cc dd aa\naa bb cc dd aaa\nabcde fghij\nabcde xyz ecdab"
-- (4,3)
solve :: [[String]] -> (Int, Int)
solve l = (length $filter valid l, length $filter valid2 l)
  where
    valid c = length c == S.size (S.fromList c)
    valid2 c = valid c && valid (freq <$> c)
    freq s = M.fromListWith (+) $ zip s (repeat 1)

main :: IO ()
main = readFile "input/day04.txt" >>= print . solve . parse
