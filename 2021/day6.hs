#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (Foldable (foldl', foldr'))
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

addToMap :: Int -> Int -> M.Map Int Int -> M.Map Int Int
addToMap v = M.alter (Just . maybe v (+ v))

-- >>> parse "3,4,3,1,2"
-- fromList [(1,1),(2,1),(3,2),(4,1)]
parse :: String -> M.Map Int Int
parse s = b
  where
    values :: [Int]
    values = map read . splitOn "," $ s
    b = foldr' (addToMap 1) M.empty values

-- >>> solve $ parse "3,4,3,1,2"
-- (5934,26984457539)
solve :: M.Map Int Int -> (Int, Int)
solve fishes = (last, last2)
  where
    nextKey k = if k == 0 then 6 else k -1
    updEntry (k, v) m = addToMap v (nextKey k) m
    nextMap m = M.insert 8 newBorn $ foldr' updEntry M.empty $ M.toList m
      where
        newBorn = M.findWithDefault 0 0 m
    generations = iterate nextMap fishes
    last = sum $ generations !! 80
    last2 = sum $ generations !! 256

main :: IO ()
main = readFile "input/day6.txt" >>= print . solve . parse
