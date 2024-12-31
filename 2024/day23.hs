#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

{-# LANGUAGE PackageImports #-}

module Main where

import Data.Foldable (foldl', maximumBy)
import Data.Function (on)
import Data.List (intercalate, sort)
import Data.Maybe (catMaybes)
import Data.Tuple (swap)
import "containers" Data.Map.Strict qualified as M
import "containers" Data.Set qualified as S

type Input = [(String, String)]

parse :: String -> Input
parse s = readline <$> lines s
  where
    readline [a, b, _, c, d] = ([a, b], [c, d])

part1 :: M.Map String (S.Set String) -> Int
part1 base = length triplets
  where
    conS a = M.findWithDefault S.empty a base
    conn a = S.toList $ conS a
    triplets = S.fromList $ filter hasT $ M.keys base >>= trs
    hasT (a : _, b : _, c : _) = 't' `elem` [a, b, c]
    trs a = [(x, y, z) | b <- conn a, c <- conn b, S.member a $ conS c, let [x, y, z] = sort [a, b, c]]

part2 :: M.Map String (S.Set String) -> String
part2 base = intercalate "," $ S.toList longest
  where
    longest = maximumBy (on compare S.size) $ S.toList groups
    groups = go S.empty $ S.singleton <$> M.keys base
    go acc b = case b of
      [] -> acc
      s : ss -> case nexts of
        [] -> go acc ss
        _ -> go acc' b'
        where
          unions l = case l of
            [] -> S.empty
            x : xs -> foldl' S.intersection x xs
          commons = unions $ (base M.!) <$> S.toList s
          toAdd = S.toList $ S.difference commons s
          nexts = filter (`S.notMember` acc) $ S.insert <$> toAdd <*> [s]
          acc' = S.union acc $ S.fromList nexts
          b' = nexts <> b

-- >>> solve $ parse "kh-tc\nqp-kh\nde-cg\nka-co\nyn-aq\nqp-ub\ncg-tb\nvc-aq\ntb-ka\nwh-tc\nyn-cg\nkh-ub\nta-co\nde-co\ntc-td\ntb-wq\nwh-td\nta-ka\ntd-qp\naq-cg\nwq-ub\nub-vc\nde-ta\nwq-aq\nwq-vc\nwh-yn\nka-de\nkh-ta\nco-tc\nwh-qp\ntb-vc\ntd-yn"
-- (7,"co,de,ka,ta")

solve :: [(String, String)] -> (Int, String)
solve l = (p1, p2)
  where
    base :: M.Map String (S.Set String)
    base = M.unionsWith (<>) [S.singleton <$> M.fromList [(a, b), (b, a)] | (a, b) <- l]
    p1 = part1 base
    p2 = part2 base

main :: IO ()
main = readFile "input/day23.txt" >>= print . solve . parse
