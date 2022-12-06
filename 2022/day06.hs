#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import qualified Data.Set as S

-- >>> solve "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
-- >>> solve "bvwbjplbgvbhsrlpgdmjqwftvncz"
-- >>> solve "nppdvjthqldpwncqszvftbrmjlhg"
-- >>> solve "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
-- >>> solve "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
-- (7,19)
-- (5,23)
-- (6,23)
-- (10,29)
-- (11,26)
solve :: String -> (Int, Int)
solve l = (p1, p2)
  where
    p1 = go l 0
    p2 = go2 l 0
    go2 s count = case S.size $ S.fromList $ take 14 s of
      14 -> count + 14
      _ -> go2 (tail s) (count + 1)
    go s count = case s of
      a : b : c : d : _ | notElem a [b, c, d] && notElem b [c, d] && c /= d -> count + 4
      _ -> go (tail s) (count + 1)

main :: IO ()
main = readFile "input/day06.txt" >>= print . solve
