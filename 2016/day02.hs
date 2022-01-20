#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (Foldable (foldl'), find)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

data Dir = U | D | L | R deriving (Show, Eq, Read, Ord)

parse :: String -> [[Dir]]
parse s = fmap (read . (: [])) <$> lines s

keypad :: M.Map (String, Dir) String
keypad = M.fromList $ M.toList rawkeypad >>= neighs
  where
    rawkeypad = M.fromList $ zip [0 ..] (lines "  1\n 234\n56789\n ABC\n  D") >>= row
    row (y, s) = [((x, y), [c]) | (x, c) <- zip [0 ..] s, c /= ' ']
    dirs = [(U, (0, -1)), (D, (0, 1)), (L, (-1, 0)), (R, (1, 0))]
    neighs ((x, y), n) = [((n, d), rawkeypad M.! idx) | (d, (dx, dy)) <- dirs, let idx = (x + dx, y + dy), M.member idx rawkeypad]

next2 :: String -> Dir -> String
next2 n d = fromMaybe n $ M.lookup (n, d) keypad

next :: Int -> Dir -> Int
next n d = case (d, mod n 3, div (n -1) 3) of
  (U, _, 0) -> n
  (U, _, _) -> n -3
  (D, _, 2) -> n
  (D, _, _) -> n + 3
  (L, 1, _) -> n
  (L, _, _) -> n -1
  (R, 0, _) -> n
  (R, _, _) -> n + 1

-- >>> solve $ parse "ULL\nRRDDD\nLURDL\nUUUUD"
-- (1985,"5DB3")

solve :: [[Dir]] -> (Int, String)
solve l = (read $ go 5 l >>= show, go2 "5" l)
  where
    go2 i d = case d of
      [] -> []
      x : xs -> i' <> go2 i' xs where i' = foldl' next2 i x
    go i d = case d of
      [] -> []
      x : xs -> i' : go i' xs where i' = foldl' next i x

main :: IO ()
main = readFile "input/day02.txt" >>= print . solve . parse
