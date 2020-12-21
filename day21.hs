#!/usr/bin/env stack
-- stack --resolver lts-10.2 script

{-# OPTIONS_GHC -Wall #-}

module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import Text.Regex.TDFA

-- >>> parse "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
-- [(fromList ["kfcds","mxmxvkd","nhms","sqjhc"],fromList ["dairy,","fish"])]

parse :: String -> [(S.Set String, S.Set String)]
parse = fmap go . lines
  where
    go :: String -> (S.Set String, S.Set String)
    go s = (ingredients, allergens)
      where
        (_, _, _, x) = s =~ "([^(]+)\\(contains ([^)]*)\\)" :: (String, String, String, [String])
        ingredients = S.fromList $ words $ head x
        allergens = S.fromList $(filter (/= ',')) <$> words (x !! 1)

-- >>> solve "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\ntrh fvjkl sbzzf mxmxvkd (contains dairy)\nsqjhc fvjkl (contains soy)\nsqjhc mxmxvkd sbzzf (contains fish)"

solve :: String -> (Int, Int)
solve s = (first, second)
  where
    first = sum [1 | fs <- fst <$> input, f <- S.toList fs, not $ S.member f potentialAllergens]
    second = 2
    input = parse s
    potentialAllergens = S.unions $ snd <$> amap
    possibleFood allergen = foldl1 S.intersection [f | (f, as) <- input, S.member allergen as]
    amap = simplify [(k, possibleFood k) | k <- S.toList $ foldl1 S.union $ snd <$> input]
    simplify al
      | null simplified = al
      | otherwise = simplify $ simplified ++ singlekv
      where
        singlekv = [(a, f) | (a, f) <- al, S.size f == 1]
        singlefoods = foldl S.union S.empty $ snd <$> singlekv
        singlekeys = S.fromList $ fst <$> singlekv
        simplified = [(a, S.difference f singlefoods) | (a, f) <- al, not $ S.member a singlekeys]

main :: IO ()
main = readFile "input/day21.txt" >>= print . solve