#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List (maximumBy)
import qualified Data.Map.Strict as M

data Reindeer = Reindeer {rdname :: String, rdspeed :: Int, rdflytime :: Int, rdRestTime :: Int} deriving (Show, Eq)

parse :: String -> [Reindeer]
parse i = pl . words <$> lines i
  where
    pl l = Reindeer (head l) (read $l !! 3) (read $l !! 6) (read $l !! 13)

travelDistance :: Reindeer -> Int -> Int
travelDistance (Reindeer _ sp ft rt) t = sp * (cycles * ft + slack)
  where
    cycles = t `div` (ft + rt)
    slack = min ft $ t `mod` (ft + rt)

-- >>> solve $ parse "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.\nDancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."
-- (2660,689)

-- [["Comet","can","fly","14","km/s","for","10","seconds,","but","then","must","rest","for","127","seconds."],["Dancer","can","fly","16","km/s","for","11","seconds,","but","then","must","rest","for","162","seconds."]]

solve :: [Reindeer] -> (Int, Int)
solve i = (p1 2503, p2 2503)
  where
    p1 t = maximum $ travelDistance <$> i <*> [t]
    p2 t = maximum $ M.unionsWith (+) $ [1 .. t] >>= point
    point s = M.singleton <$> winners <*> [1]
      where
        scores = zip (rdname <$> i) $ travelDistance <$> i <*> [s]
        maxScore = maximum $ snd <$> scores
        winners = fst <$> filter ((== maxScore) . snd) scores

main :: IO ()
main = readFile "input/day14.txt" >>= print . solve . parse
