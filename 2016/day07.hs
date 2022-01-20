#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List (isInfixOf, partition, sortBy, sortOn, transpose)
import Data.List.Split (splitWhen)
import qualified Data.Map.Strict as M

parse :: String -> [[String]]
parse s = splitWhen (`elem` "[]") <$> lines s

-- >>> solve $ parse "abba[mnop]qrst\nabcd[bddb]xyyx\naaaa[qwer]tyui\nioxxoj[asdfgh]zxcvbn"
-- >>> solve $ parse "aba[bab]xyz\nxyx[xyx]xyx\naaa[kek]eke\nzazbz[bzb]cdb"
-- (2,0)
-- (0,3)

-- solve :: [[String]] -> (String, String)
solve l = (length $filter valid l, length $filter valid2 l)
  where
    oddeven l = (snd <$> o, snd <$> e) where (o, e) = partition (odd . fst) $ zip [1 ..] l
    valid2 l = or $ isInfixOf <$> bb <*> e
      where
        (o, e) = oddeven l
        bb = o >>= babs

    valid l = any hasAbba e && not (any hasAbba o) where (e, o) = oddeven l

    babs s = case s of
      a : b : c : xs
        | a == c && a /= b -> [b, a, b] : babs (b : c : xs)
        | otherwise -> babs (b : c : xs)
      _ -> []

    hasAbba s = case s of
      a : b : c : d : _
        | a == d && b == c && c /= d -> True
        | otherwise -> hasAbba $ tail s
      _ -> False

main :: IO ()
main = readFile "input/day07.txt" >>= print . solve . parse
