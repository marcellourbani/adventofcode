#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (foldr')
import Data.List (sortOn)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

type Input = (String, M.Map String String)

parse :: String -> Input
parse s = (ini, foldr' conv M.empty $ splitOn " -> " <$> rest)
  where
    (ini : _ : rest) = lines s
    conv [k, v] m = M.insert k v m
    conv _ m = m

step :: Input -> Input
step (ini, m) = (go ini, m)
  where
    pins k = case (k, M.lookup k m) of
      (b : _, Just a) -> b : a
      (b : _, _) -> [b]
      _ -> []
    go s = case s of
      (a : b : rest) -> pins [a, b] ++ go (b : rest)
      [a] -> s
      _ -> []

-- >>> solve  $parse "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C"
-- (1588,0)

solve :: Input -> (Int, Int)
solve i = (mostfreq - leastfreq, 0)
  where
    steps = iterate step i
    final = fst $ steps !! 10
    frequencies = M.toList $ foldr' (M.alter (Just . maybe 1 (+ 1))) M.empty final
    sortedfreq = sortOn snd frequencies
    leastfreq = snd . head $ sortedfreq
    mostfreq = snd . last $sortedfreq

main :: IO ()
main = readFile "input/day14.txt" >>= print . solve . parse
