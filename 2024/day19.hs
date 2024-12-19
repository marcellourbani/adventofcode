#!/usr/bin/env stack
-- stack --resolver lts-20.26 script --optimize

{-# LANGUAGE PackageImports #-}

module Main where

import Data.Foldable (Foldable (foldl'))
import Data.List (isPrefixOf, partition, sort)
import Data.List.Split (divvy, splitOn)
import Data.Maybe (mapMaybe)
import Data.PQueue.Prio.Min qualified as P
import Linear.V2
import "containers" Data.Map.Strict qualified as M

type Input = ([String], [String])

parse :: String -> Input
parse s = (splitOn ", " t, lines p) where [t, p] = splitOn "\n\n" s

shortestPath :: (Num n, Ord n, Ord state) => state -> (state -> [(state, n)]) -> (state -> Bool) -> Maybe (n, [state])
shortestPath initial nexts isGoal = case go M.empty iq M.empty of
  Nothing -> Nothing
  Just (target, dist, prevs) -> Just (dist, target : findpath prevs target)
  where
    iq = P.singleton 0 (initial, Nothing)
    findpath prevs goal = case prevs M.!? goal of
      Nothing -> [goal]
      Just cur -> cur : findpath prevs cur
    go nodes queue prevs = case P.getMin queue of
      Nothing -> Nothing
      Just (_, (cur, _)) | M.member cur nodes -> go nodes (P.deleteMin queue) prevs
      Just (curdist, (cur, prev))
        | isGoal cur -> Just (cur, curdist, prevs')
        | otherwise -> go nodes' queue' prevs'
        where
          dists (s, cost) = (curdist + cost, (s, Just cur))
          newentries = dists <$> filter ((`M.notMember` nodes) . fst) (nexts cur)
          queue' = P.union queue $ P.fromList newentries
          nodes' = M.insert cur curdist nodes
          prevs' = case prev of
            Just p -> M.insert cur p prevs
            _ -> prevs

possible :: [String] -> String -> Maybe (Int, [([String], Int)])
possible !patterns !goal = shortestPath ([], 0) nexts isGoal
  where
    lg = length goal
    maxl = 1 + maximum (length <$> patterns)
    isGoal (!s, !c) = lg == c
    vp !c = filter (`isPrefixOf` drop c goal) patterns
    nexts (!s, !c) = [((s', c + length p), maxl - length p) | p <- vp c, let s' = s <> [p]]

minimump :: [String] -> ([String], M.Map [String] String)
minimump !pats = go (pl 1) 1 [] []
  where
    maxl = maximum (length <$> pats)
    pl n = filter ((== n) . length) pats
    go l n ps cs = case l of
      [] | n == maxl -> (ps, M.fromList cs)
      [] -> go (pl $ n + 1) (n + 1) ps cs
      x : xs -> case possible ps x of
        Nothing -> go xs n (x : ps) cs
        Just (_, (p, _) : _) -> go xs n ps $ (p, x) : cs

parts :: [(Int, Int)] -> Int -> [(Int, Int)] -> ([(Int, Int)], [(Int, Int)])
parts l x acc = case l of
  [] -> (acc, [])
  (a, b) : ls
    | a <= x -> parts ls x $ (a, b) : acc
    | otherwise -> (acc, l)

part2 :: M.Map [String] String -> [[String]] -> Int
part2 comps base = sum $ procline <$> base
  where
    nums = [2 .. maximum $ length <$> M.keys comps]
    procline a = sum $ snd <$> (foldl' nexts [(0, 1)] . ks) a
    ks b = sort $ M.toList $ M.unionsWith (<>) [M.singleton o [l] | l <- nums, (o, k) <- zip [2 ..] $ divvy l 1 b, M.member k comps]

nexts :: [(Int, Int)] -> (Int, [Int]) -> [(Int, Int)]
nexts o (ofs, plen) = news
  where
    (los, res) = partition ((<= ofs) . fst) o
    nv = sum (snd <$> los)
    nof = (+ ofs) <$> plen
    news = if nv == 0 then o else o <> ((,nv) <$> nof)

-- >>> solve $ parse "r, wr, b, g, bwu, rb, gb, br\n\nbrwrr\nbggr\ngbbr\nrrbgbr\nubwu\nbwurrg\nbrgr\nbbrgwb"
-- (6,16)

solve :: ([String], [String]) -> (Int, Int)
solve (pats, des) = (p1, p2)
  where
    (minp, parts) = minimump pats
    base = fst . head . snd <$> mapMaybe (possible minp) des
    p1 = length base
    p2 = part2 parts base

main :: IO ()
main = readFile "input/day19.txt" >>= print . solve . parse
