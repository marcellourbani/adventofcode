#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Maybe (fromMaybe)
import qualified Data.Sequence as S

parse :: String -> Int
parse = read

-- >>> solve $ parse "5"
-- (3,2)

solve :: Int -> (Int, Int)
solve n = (go l, p2' (S.fromList l) 0)
  where
    l = [1 .. n]
    go elfs
      | ne == 1 = head elfs
      | even ne = go $ go2 elfs
      | otherwise = go $ tail (go2 notlast) <> lastelf
      where
        ne = length elfs
        (notlast, lastelf) = splitAt (ne -1) elfs
        go2 el = case el of
          (x : y : xs) -> x : go2 xs
          _ -> []

    p2' el n
      | numel == 1 = fromMaybe 0 $ S.lookup 0 el
      | otherwise = p2' el' n'
      where
        numel = S.length el
        mid = mod (n + div numel 2) numel
        n'
          | n <= mid = mod (n + 1) (numel - 1)
          | n < numel -1 = n
          | otherwise = 0
        (pre, pos) = S.splitAt mid el
        el' = pre <> S.drop 1 pos

main :: IO ()
main = readFile "input/day19.txt" >>= print . solve . parse
