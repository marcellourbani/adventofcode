#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List (findIndex)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

type Mutations = M.Map String [String]

type Input = (String, Mutations)

parse :: String -> Input
parse i = (s, S.toList <$> mutations)
  where
    mutations = M.unionsWith S.union $ pl <$> lines ls
    [ls, s] = splitOn "\n\n" i
    pl l = M.singleton f $ S.singleton t where [f, t] = splitOn " => " l

mutateOne :: String -> Mutations -> S.Set String
mutateOne s m = S.delete s $ S.fromList $ go s
  where
    go s = case s of
      [] -> []
      (x : y : xs) | M.member [x, y] m -> repl [x, y] xs <> others [x, y] xs
      (x : xs) | M.member [x] m -> repl [x] xs <> others [x] xs
      (x : xs) -> (x :) <$> go xs
      where
        repl k s = (<> s) <$> M.findWithDefault [] k m
        others k xs = (k <>) <$> go xs

-- >>> solve $ parse "e => H\ne => O\nH => HO\nH => OH\nO => HH\nH => HO\nH => OH\nO => HH\n\nHOHOHO"
-- 7

solve :: Input -> Int
solve (s, m) = length $ mutateOne s m

main :: IO ()
main = readFile "input/day19.txt" >>= print . solve . parse
