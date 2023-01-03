#!/usr/bin/env stack
-- stack --resolver lts-18.18 script --package data-clist

module Main where

import qualified Data.CircularList as C
import Data.Foldable (Foldable (toList))
import Data.List (elemIndex, find, tails)
import Data.Maybe (catMaybes, fromMaybe)

-- import qualified Data.Sequence as S

parse :: String -> [Int]
parse s = read <$> lines s

groveC :: C.CList Int -> Int
groveC l = case C.findRotateTo (== 0) l of
  Nothing -> 0
  Just cl -> sum $ catMaybes $ fmap C.focus . C.rotNR <$> idxs <*> [cl]
    where
      idxs = mod <$> [1000, 2000, 3000] <*> [C.size cl]

mix :: C.CList (Int, Int) -> Int -> C.CList (Int, Int)
mix cl i = case C.findRotateTo ((== i) . fst) cl of
  Nothing -> cl
  Just cl' -> case C.focus cl' of
    Nothing -> cl
    Just c@(_, shi) -> C.insertL c $ C.rotNR shi' $ C.removeL cl'
      where
        shi' = mod shi $ C.size cl - 1

-- >>> solve $ parse "1\n2\n-3\n3\n-2\n0\n4"
-- ([4,-3,2],[811589153,2434767459,-1623178306])

solve :: [Int] -> (Int, Int)
solve l = (p1, p2)
  where
    initial = C.fromList $ zip [0 ..] l
    initial2 = C.fromList $ zip [0 ..] $ (* 811589153) <$> l
    p1 = groveC $ snd <$> go initial [0 .. length l]
    p2 = groveC $ snd <$> go initial2 (concat $ replicate 10 [0 .. length l])
    go ll is = case is of
      [] -> ll
      x : xs -> go (mix ll x) xs

main :: IO ()
main = readFile "input/day20.txt" >>= print . solve . parse
