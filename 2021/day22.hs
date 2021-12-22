#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (Foldable (foldl'))
import Data.List.Split (splitOn)
import qualified Data.Set as S

type Range = (Int, Int)

type Instruction = (Bool, Range, Range, Range)

type Cores = S.Set (Int, Int, Int)

type Input = [Instruction]

parse :: String -> Input
parse s = ls
  where
    ls = pl <$> lines s
    pl ss = (c == "on", pr x, pr y, pr z)
      where
        [[c], [x, y, z]] = splitOn "," <$> splitOn " " ss
    pr ss = (read f, read t)
      where
        [f, t] = splitOn ".." $ tail $ dropWhile (/= '=') ss

isValidC :: Range -> Bool
isValidC (f, t) = abs f <= 50 && abs t <= 50

isValid :: Instruction -> Bool
isValid (_, x, y, z) = isValidC x && isValidC y && isValidC z

step :: Cores -> Instruction -> Cores
step c (op, (x1, x2), (y1, y2), (z1, z2)) = if op then S.union c ckeys else S.difference c ckeys
  where
    ckeys = S.fromList [(x, y, z) | x <- [x1 .. x2], y <- [y1 .. y2], z <- [z1 .. z2]]

-- >>> solve  $parse "on x=-20..26,y=-36..17,z=-47..7\non x=-20..33,y=-21..23,z=-26..28\non x=-22..28,y=-29..23,z=-38..16\non x=-46..7,y=-6..46,z=-50..-1\non x=-49..1,y=-3..46,z=-24..28\non x=2..47,y=-22..22,z=-23..27\non x=-27..23,y=-28..26,z=-21..29\non x=-39..5,y=-6..47,z=-3..44\non x=-30..21,y=-8..43,z=-13..34\non x=-22..26,y=-27..20,z=-29..19\noff x=-48..-32,y=26..41,z=-47..-37\non x=-12..35,y=6..50,z=-50..-2\noff x=-48..-32,y=-32..-16,z=-15..-5\non x=-18..26,y=-33..15,z=-7..46\noff x=-40..-22,y=-38..-28,z=23..41\non x=-16..35,y=-41..10,z=-47..6\noff x=-32..-23,y=11..30,z=-14..3\non x=-49..-5,y=-3..45,z=-29..18\noff x=18..30,y=-20..-8,z=-3..13\non x=-41..9,y=-7..43,z=-33..15\non x=-54112..-39298,y=-85059..-49293,z=-27449..7877\non x=967..23432,y=45373..81175,z=27513..53682"
-- 590784

solve :: Input -> Int
solve i = S.size p1cores
  where
    p1input = filter isValid i
    p1cores = foldl' step S.empty p1input

main :: IO ()
main = readFile "input/day22.txt" >>= print . solve . parse
