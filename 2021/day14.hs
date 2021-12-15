#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (foldr')
import Data.List (sortOn)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)

type Input = (String, M.Map String String)

parse :: String -> Input
parse s = (ini, foldr' conv M.empty $ splitOn " -> " <$> rest)
  where
    (ini : _ : rest) = lines s
    conv [k, v] m = M.insert k v m
    conv _ m = m

calcFreq :: Input -> Int -> M.Map Char Int
calcFreq (ini, m) lev = M.unionWith (-) totalfreq duplicates
  where
    totalfreq = M.unionsWith (+) $ catMaybes $ M.lookup <$> couples ini <*> pure finalLevel
    couples s = case s of
      (a : b : rest) -> [a, b] : couples (b : rest)
      _ -> []
    duplicates = snd . freqs $ tail $ reverse $ tail ini
    finalLevel = levels !! lev
    levelzero = M.fromList $ freqs <$> M.keys m
    freqs s = (s, M.unionsWith (+) $ M.fromList . (: []) <$> zip s (repeat 1))
    levels = iterate nextLevel levelzero
    nextLevel clevel = M.fromList $ expand <$> M.toList clevel
      where
        expand (k, v) = case (k, M.lookup k m) of
          ([a, b], Just [c]) -> (k, v')
            where
              v' = case (M.lookup [a, c] clevel, M.lookup [c, b] clevel) of
                (Just fp, Just fs) -> M.adjust (+ (-1)) c $ M.unionWith (+) fp fs
                (Just fp, _) -> M.adjust (+ 1) b fp
                (_, Just fs) -> M.adjust (+ 1) a fs
                _ -> v
          _ -> (k, v)

-- >>> solve  $parse "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C"
-- (1588,2188189693529)

solve :: Input -> (Int, Int)
solve i = (go 10, go 40)
  where
    go n = mostfreq - leastfreq
      where
        freqs = sortOn snd (M.toList $ calcFreq i n)
        leastfreq = snd . head $ freqs
        mostfreq = snd . last $freqs

main :: IO ()
main = readFile "input/day14.txt" >>= print . solve . parse
