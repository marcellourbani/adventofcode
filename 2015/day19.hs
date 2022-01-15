#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List (findIndex)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

type Mutations = M.Map String [[String]]

type Input = ([String], Mutations)

parse :: String -> Input
parse i = (toTokens s, S.toList <$> mutations)
  where
    uppers = S.fromList ['A' .. 'Z']
    toTokens s = case s of
      [] -> []
      c1 : c2 : cs | S.notMember c2 uppers -> [c1, c2] : toTokens cs
      c : cs -> [c] : toTokens cs
    mutations = M.unionsWith S.union $ pl <$> lines ls
    [ls, s] = splitOn "\n\n" i
    pl l = M.singleton f $ S.singleton $ toTokens t where [f, t] = splitOn " => " l

mutateOne :: [String] -> Mutations -> S.Set [String]
mutateOne s m = S.delete s $ S.fromList $ go s
  where
    go s = case s of
      [] -> []
      (x : xs) | M.member x m -> repl x xs <> others [x] xs
      (x : xs) -> (x :) <$> go xs
      where
        repl k s = (<> s) <$> M.findWithDefault [] k m
        others k xs = (k <>) <$> go xs

-- for part2, production rules all look like:
-- H => C F  -- adds one to the length
-- H => C Rn F Ar -- adds 3
-- H => C Rn F Y F Ar --adds 5
-- H => C Rn F Y F YF Ar -- adds 7
-- so basically the answer is total length -1 - num(Ar) -num (Rn) - 2* num (Y)
-- only found out in reddit :(

-- >>> solve $ parse "e => H\ne => O\nH => HO\nH => OH\nO => HH\nH => HO\nH => OH\nO => HH\n\nHOHOHO"
-- 7

solve :: Input -> (Int, Int)
solve (s, m) = (length $ mutateOne s m, length s - 2 * seps -1)
  where
    seps = length $ filter (`elem` ["Y", "Rn"]) s

main :: IO ()
main = readFile "input/day19.txt" >>= print . solve . parse
