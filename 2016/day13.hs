#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE TupleSections #-}

module Main where

import Control.Arrow (first)
import Data.Bits
import Data.Foldable (foldl')
import qualified Data.PQueue.Min as P
import qualified Data.Set as S

parse :: String -> (Int, (Int, Int))
parse s = (read s, (31, 39))

isOpen :: Int -> (Int, Int) -> Bool
isOpen a (x, y) = even . popCount $ a + x * x + 3 * x + 2 * x * y + y + y * y

-- >>> solve (10,(7,4))
-- (11,151)

solve :: (Int, (Int, Int)) -> (Int, Int)
solve (l, g) = (p1, S.size $ expand 50 (S.singleton (1, 1)) (S.singleton (1, 1)))
  where
    p1 = djkstra p1isGoal p1nexts S.empty initial
    initial = P.singleton (0, (1, 1))
    p1isGoal s = s == g
    p1nexts (a, b) = [(1, (x, y)) | (x, y) <- ((a,) <$> [b -1, b + 1]) <> ((,b) <$> [a -1, a + 1]), x >= 0, y >= 0, isOpen l (x, y)]
    expand n locations acc = case (n, newLocations) of
      (0, _) -> acc
      (_, s) | s == S.empty -> acc
      _ -> expand (n -1) newLocations acc'
      where
        news = S.fromList $ S.toList locations >>= (map snd . p1nexts)
        newLocations = S.difference news acc
        acc' = S.union acc newLocations
    djkstra isGoal nexts visited queue
      | isGoal cur = cost
      | S.member cur visited = djkstra isGoal nexts visited queue'
      | otherwise = djkstra isGoal nexts visited' queue''
      where
        ((cost, cur), queue') = P.deleteFindMin queue
        visited' = S.insert cur visited
        neighs = first (+ cost) <$> filter ((`S.notMember` visited') . snd) (nexts cur)
        queue'' = foldl' (flip P.insert) queue' neighs

main :: IO ()
main = readFile "input/day13.txt" >>= print . solve . parse
