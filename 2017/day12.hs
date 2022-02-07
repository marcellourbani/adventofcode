#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S

parse :: String -> M.Map Int (S.Set Int)
parse s = M.unionsWith S.union $ pl <$> lines s
  where
    pl l = M.fromList $ zip bs (S.singleton <$> ts) <> zip ts (S.singleton <$> bs)
      where
        [b, n] = splitOn " <-> " l
        ts = read <$> splitOn ", " n
        bs = repeat $ read b

-- >>> solve $ parse "0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5"
-- (6,2)
solve :: M.Map Int (S.Set Int) -> (Int, Int)
solve i = (S.size $ group i 0, p2 i)
  where
    p2 m
      | M.null m = 0
      | otherwise = 1 + p2 m'
      where
        x = fst $ M.findMin m
        gx = group m x
        m' = M.filterWithKey (const . (`S.notMember` gx)) m
    group m i = go m [i] $ S.singleton i
    go m l s = case l of
      [] -> s
      _ -> go m l'' s'
      where
        l' = S.unions $ catMaybes $ M.lookup <$> l <*> [m]
        l'' = S.toList $ S.difference l' s
        s' = S.union s l'

main :: IO ()
main = readFile "input/day12.txt" >>= print . solve . parse
