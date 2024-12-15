#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

module Main where

import Control.Lens ((^.))
import Data.List (elemIndex)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Linear.V2

data Machine = Machine {mA :: V2 Int, mB :: V2 Int, mPrize :: V2 Int} deriving (Show)

type Input = [Machine]

parse :: String -> Input
parse s = parseM <$> i
  where
    i = splitOn [""] $ lines s
    parseM (a : b : p : _) = Machine (V2 xa ya) (V2 xb yb) (V2 xp yp)
      where
        xa = read $ take 2 $ drop 12 a
        ya = read $ take 2 $ drop 18 a
        xb = read $ take 2 $ drop 12 b
        yb = read $ take 2 $ drop 18 b
        [xp, yp] = read <$> splitOn ", Y=" (drop 9 p)

solveMachine :: Machine -> Maybe (Int, Int)
solveMachine (Machine a@(V2 a1 a2) b@(V2 b1 b2) p@(V2 p1 p2))
  | rx == 0 && ry == 0 = Just (x, y)
  | otherwise = Nothing
  where
    (x, rx) = divMod (p2 * b1 - p1 * b2) (a2 * b1 - a1 * b2)
    (y, ry) = divMod (p1 - x * a1) b1

part1 :: Input -> Int
part1 l = sum $ cost <$> mapMaybe solveMachine l
  where
    cost (a, b) = 3 * a + b

part2 :: Input -> Int
part2 l = part1 $ convert <$> l
  where
    convert m@(Machine _ _ p) = m {mPrize = p + V2 10000000000000 10000000000000}

-- >>> solve $ parse "Button A: X+94, Y+34\nButton B: X+22, Y+67\nPrize: X=8400, Y=5400\n\nButton A: X+26, Y+66\nButton B: X+67, Y+21\nPrize: X=12748, Y=12176\n\nButton A: X+17, Y+86\nButton B: X+84, Y+37\nPrize: X=7870, Y=6450\n\nButton A: X+69, Y+23\nButton B: X+27, Y+71\nPrize: X=18641, Y=10279"
-- (480,875318608908)

solve :: Input -> (Int, Int)
solve l = (p1, p2)
  where
    p1 = part1 l
    p2 = part2 l

main :: IO ()
main = readFile "input/day13.txt" >>= print . solve . parse
