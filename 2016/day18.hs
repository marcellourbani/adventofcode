-- stack --resolver lts-18.18 script

{-# LANGUAGE TupleSections #-}

module Main where

import Data.Foldable (Foldable (foldl'))
import qualified Data.Map.Strict as M

parse :: String -> [Bool]
parse s = (== '.') <$> s

isSafe :: [Bool] -> Bool
isSafe pat = case pat of
  [False, False, True] -> False
  [True, False, False] -> False
  [False, True, True] -> False
  [True, True, False] -> False
  _ -> True

createMap :: [Bool] -> Int -> M.Map (Int, Int) Bool
createMap first lines = M.filter id $ foldl' addline firstlinemap [2 .. lines]
  where
    firstlinemap = M.fromList $ zip (zip [1 ..] $repeat 1) first
    wid = length first
    prevSafe m (x, y) = isSafe $ M.findWithDefault True . (,y -1) <$> [x -1 .. x + 1] <*> pure m
    addline m l = M.union m $ M.fromList newline
      where
        cs = zip [1 .. wid] $ repeat l
        newline = zip cs $ prevSafe m <$> cs

-- creating a 4000000 line map was fast enough, but this is way faster
countSafe :: [Bool] -> Int -> Int
countSafe first lines = go first 1
  where
    nextLine line = go2 $ [True] <> line <> [True]
      where
        go2 l = case l of
          a : b : c : xs -> isSafe [a, b, c] : go2 (tail l)
          _ -> []
    go l i
      | i == lines = cur
      | otherwise = cur + go next (i + 1)
      where
        cur = length $ filter id l
        next = nextLine l

-- >>> length $ createMap (parse ".^^.^.^^^^") 10
-- 38

solve :: [Bool] -> (Int, Int)
solve l = (length $ createMap l 40, countSafe l 400000)

main :: IO ()
main = readFile "input/day18.txt" >>= print . solve . parse
