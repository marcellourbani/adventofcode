#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (find)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Inventory = M.Map String Int

type Input = M.Map Int Inventory

parse :: String -> Input
parse i = M.unions $pl . words <$> lines (filter (`notElem` ",:") i)
  where
    pl l = M.singleton (read $l !! 1) $ M.fromList (pr $ drop 2 l)
    pr l = case l of
      (label : count : rest) -> (label, read count) : pr rest
      _ -> []

target :: Inventory
target = M.fromList [("children", 3), ("cats", 7), ("samoyeds", 2), ("pomeranians", 3), ("akitas", 0), ("vizslas", 0), ("goldfish", 5), ("trees", 3), ("cars", 2), ("perfumes", 1)]

-- >>> solve $ parse "Sue 171: perfumes: 3, goldfish: 10, cats: 3\nSue 40: vizslas: 0, cats: 7, akitas: 0\nSue 241: cars: 2, pomeranians: 1, samoyeds: 2"
-- (40,241)

solve :: Input -> (Int, Int)
solve l = (p1, p2)
  where
    p1 = maybe 0 fst $ find (valid . snd) $ M.toList l
    p2 = maybe 0 fst $ find (valid2 . snd) $ M.toList l
    exceptions = S.fromList ["cats", "trees", "pomeranians", "goldfish"]
    partialTarget = M.filterWithKey (const . (`S.notMember` exceptions)) target
    valid i = M.intersection target i == i
    valid2 i = partial && cats && trees && pomeranians && goldfish
      where
        partial = M.intersection partialTarget i == M.intersection i partialTarget
        cats = maybe True (> 7) $ M.lookup "cats" i
        trees = maybe True (> 3) $ M.lookup "trees" i
        pomeranians = maybe True (< 3) $ M.lookup "pomeranians" i
        goldfish = maybe True (< 5) $ M.lookup "goldfish" i

main :: IO ()
main = readFile "input/day16.txt" >>= print . solve . parse
