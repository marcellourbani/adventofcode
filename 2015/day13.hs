#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List (permutations)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

parse :: String -> M.Map (String, String) Int
parse i = M.fromList $ pl <$> lines (filter (/= '.') i)
  where
    pl l = ((f, t), read v * if s == "gain" then 1 else -1) where (f : _ : s : v : _ : _ : _ : _ : _ : _ : t : _) = words l

-- >>> solve $ parse "Alice would gain 54 happiness units by sitting next to Bob.\nAlice would lose 79 happiness units by sitting next to Carol.\nAlice would lose 2 happiness units by sitting next to David.\nBob would gain 83 happiness units by sitting next to Alice.\nBob would lose 7 happiness units by sitting next to Carol.\nBob would lose 63 happiness units by sitting next to David.\nCarol would lose 62 happiness units by sitting next to Alice.\nCarol would gain 60 happiness units by sitting next to Bob.\nCarol would gain 55 happiness units by sitting next to David.\nDavid would gain 46 happiness units by sitting next to Alice.\nDavid would lose 7 happiness units by sitting next to Bob.\nDavid would gain 41 happiness units by sitting next to Carol."
-- (330,286)

solve m = (maximum $ score <$> uperms names, maximum $ score <$> uperms ("Me!" : names))
  where
    names = S.toList $ S.fromList $ fst <$> M.keys m
    score l = sum $ pscore <$> pairs l
    pscore k@(a, b) = fromMaybe 0 (M.lookup k m) + fromMaybe 0 (M.lookup (b, a) m)
    pairs l = take (length l) $ zip ll $ tail ll where ll = cycle l
    uperms l = case l of
      [] -> []
      x : xs -> (x :) <$> permutations xs

main :: IO ()
main = readFile "input/day13.txt" >>= print . solve . parse
