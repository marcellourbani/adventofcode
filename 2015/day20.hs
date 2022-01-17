#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (find, foldr')
import qualified Data.Map.Strict as M
import Debug.Trace (trace)

parse :: String -> Int
parse = read

-- >>> solve $ parse "150"
-- 8

-- solve :: Int -> Int
solve l = minimum $ M.keys sols
  where
    target = div l 10
    sols = M.filter (>= target) $ foldr' f M.empty [1 .. target `div` 2 + 1]
    f x y = M.unionWith (+) y nm where nm = M.fromList $ zip [x, x + x .. target] $ repeat x

main :: IO ()
main = readFile "input/day20.txt" >>= print . solve . parse
