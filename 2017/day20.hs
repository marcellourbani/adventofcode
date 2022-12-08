#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.List.Split (splitOn)

type Vector = (Int, Int, Int)

data Particle = Particle {pos :: Vector, vel :: Vector, acc :: Vector} deriving (Show)

parse :: String -> [Particle]
parse s = pl <$> lines s
  where
    sl l = splitOn ", " $filter (`notElem` "<>pva=") l
    pt t = (a, b, c) where [a, b, c] = read <$> splitOn "," t
    pl l = Particle a b c where [a, b, c] = pt <$> sl l

manhattan :: Vector -> Int
manhattan (x, y, z) = sum $ abs <$> [x, y, z]

-- >>> solve $ parse "p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>\np=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>"
-- 4
solve l = p2
  where
    accs = zip [0 ..] $manhattan . acc <$> l
    p1 = fst $ minimumBy (on compare snd) accs -- should check speeds on particles with same acceleration, but puzzle doesn't require it
    p2 = maximum $ manhattan . vel <$> l

-- closest =

main :: IO ()
main = readFile "input/day20.txt" >>= print . solve . parse
