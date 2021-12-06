#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (Foldable (foldl', foldr'))
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

-- >>> parse "3,4,3,1,2"
-- fromList [(1,1),(2,1),(3,2),(4,1)]
parse :: String -> M.Map Int Int
parse s = b
  where
    values :: [Int]
    values = map read . splitOn "," $ s
    f = M.alter (Just . maybe 1 (+ 1))
    a = f 2 M.empty
    b = foldr' f M.empty values

-- >>> solve $ parse "3,4,3,1,2"
-- (5934,26984457539)
solve :: M.Map Int Int -> (Int, Int)
solve fishes = (last,last2)
  where
    nextGen ts = M.fromList base
      where
        base = go [0 .. 8] []
        go i acc = case i of
          [] -> acc
          0 : ks -> go ks $(6,val 0+val 7):(8,val 0):acc
          7:ks -> go ks acc
          k : ks -> go ks $(k-1,val k):acc
          where
            val k = fromMaybe 0 $ M.lookup k ts
    generations = iterate nextGen fishes
    last = sum $ generations !! 80
    last2 = sum $ generations !! 256

main :: IO ()
main = readFile "input/day6.txt" >>= print . solve . parse
