#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List.Split (splitOn)
import Data.Maybe (isNothing)

data CondRecord = CondRecord String [Int] deriving (Show)

parse :: String -> [CondRecord]
parse s = parseLine <$> lines s
  where
    parseLine l = case words l of
      [a, b] -> CondRecord a $ read <$> splitOn "," b
      _ -> error "bad input"

-- >>> expand $ CondRecord "????.######..#####." [6,5]
-- [CondRecord ".....######..#####." [6,5]]

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

-- >>> solve $ parse "???.### 1,1,3\n.??..??...?##. 1,1,3\n?#?#?#?#?#?#?#? 1,3,1,6\n????.#...#... 4,1,1\n????.######..#####. 1,6,5\n?###???????? 3,2,1"
-- (21,1)
solve :: [CondRecord] -> Int
solve l = p1
  where
    p1 = sum $ length . expand <$> l

main :: IO ()
main = readFile "input/day12.txt" >>= print . solve . parse
