#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (isJust, isNothing, listToMaybe)

data CondRecord = CondRecord String [Int] deriving (Show)

parse :: String -> [CondRecord]
parse s = parseLine <$> lines s
  where
    parseLine l = case words l of
      [a, b] -> CondRecord a $ read <$> splitOn "," b
      _ -> error "bad input"

expand :: CondRecord -> [CondRecord]
expand (CondRecord spr lengths) = go "" spr lengths
  where
    go pref s le = case (s, le) of
      (_, []) | '#' `elem` s -> []
      (_, []) -> [CondRecord (pref ++ replicate (length s) '.') lengths]
      ([], _) -> []
      (c : cs, x : xs)
        | remaining < x -> []
        | c == '.' -> others
        | c == '#' && not matchP -> []
        | c == '#' && null s' -> go pref' s' xs
        | c == '#' && head s' == '#' -> []
        | c == '#' -> go pref'' (tail s') xs
        | matchP && null s' -> others <> go pref' s' xs
        | matchP && head s' == '#' -> others
        | matchP -> others <> go pref'' (tail s') xs
        | otherwise -> others
        where
          remaining = length s
          others = go (pref ++ ".") cs le
          s' = drop x s
          pref' = pref <> replicate x '#'
          pref'' = pref' <> "."
          matchP = all (`elem` "#?") (take x s)

dfscount :: CondRecord -> Int
dfscount (CondRecord spr lengths) = fst $ go M.empty spr lengths
  where
    mytail r = if null r then r else tail r

    dropdots s = case s of
      '.' : cs -> dropdots cs
      _ -> s

    match s b = (valid, pre, dropdots $ mytail rest)
      where
        (pre, rest) = splitAt b s
        valid = length pre == b && '.' `notElem` pre && (listToMaybe rest /= Just '#')

    matches s b = case s of
      [] -> []
      '.' : cs -> matches cs b
      '#' : cs -> case match s b of
        (False, pre, rest) -> []
        (True, pre, rest) -> [rest]
      _ -> case match s b of
        (False, pre, rest) -> matches (mytail s) b
        (True, pre, rest) -> rest : matches (mytail s) b
    subs bs l c acc = case l of
      [] -> (acc, c)
      x : xs -> subs bs xs c' (acc + n)
        where
          (n, c') = go c x bs
    go cache s le = case (le, cached) of
      ([], _) | '#' `elem` s -> (0, cache)
      ([], _) -> (1, cache)
      (_, Just n) -> (n, cache)
      (b : bs, _) -> (n, cache')
        where
          ms = matches s b
          (n, c') = subs bs ms cache 0
          cache' = M.insert (length s, length le) n c'
      where
        cached = M.lookup (length s, length le) cache

extend :: CondRecord -> CondRecord
extend (CondRecord s l) = CondRecord (s <> "?" <> s <> "?" <> s <> "?" <> s <> "?" <> s) (l <> l <> l <> l <> l)

-- >>> solve $ parse "???.### 1,1,3\n.??..??...?##. 1,1,3\n?#?#?#?#?#?#?#? 1,3,1,6\n????.#...#... 4,1,1\n????.######..#####. 1,6,5\n?###???????? 3,2,1"
-- (21,525152)

solve :: [CondRecord] -> (Int, Int)
solve l = (p1, p2)
  where
    p1 = sum $ length . expand <$> l
    p2 = sum $ dfscount <$> (extend <$> l)

-- p2 = (extend <$> l)

main :: IO ()
main = readFile "input/day12.txt" >>= print . solve . parse
