#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import Control.Lens ((^.))
import Data.Bits (xor)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Ratio ((%))
import qualified Data.Set as S
import Linear.V3

data HailStone = HailStone {hsP :: V3 Rational, hsV :: V3 Rational} deriving (Eq, Show)

data Trajectory = Trajectory {lA :: Rational, lB :: Rational, lX0 :: Rational, lForward :: Bool} deriving (Eq, Show)

parse :: String -> [HailStone]
parse s = parseline . map (fromInteger . read) . words <$> lines (filter (`notElem` ",@") s)
  where
    parseline [px, py, pz, vx, vy, vz] = HailStone (V3 px py pz) (V3 vx vy vz)

trajectory :: HailStone -> Trajectory
trajectory (HailStone (V3 x y _) (V3 vx vy _)) = Trajectory a b x (vx >= 0) where a = vy / vx; b = y - a * x

lineAt :: Trajectory -> Rational -> Rational
lineAt (Trajectory a b _ _) x = a * x + b

lineIntersection :: Trajectory -> Trajectory -> Maybe Rational
lineIntersection t1@(Trajectory a1 b1 _ _) t2@(Trajectory a2 b2 _ _) =
  case a1 == a2 of
    True -> Nothing
    _
      | inPast t1 || inPast t2 -> Nothing
      | otherwise -> Just ((b2 - b1) / (a1 - a2))
      where
        inPast (Trajectory _ _ x0 f) = xor f $ i > x0
        i = (b2 - b1) / (a1 - a2)

-- >>> solve 7 27 $ parse "19, 13, 30 @ -2,  1, -2\n18, 19, 22 @ -1, -1, -2\n20, 25, 34 @ -2, -2, -4\n12, 31, 28 @ -1, -2, -1\n20, 19, 15 @  1, -5, -3"
-- 2
solve :: Rational -> Rational -> [HailStone] -> Int
solve mi ma l = p1
  where
    h = head l
    x1 = hsP h ^. _x
    ts = trajectory <$> l
    inlimits (x, y) = x <= ma && x >= mi && y <= ma && y >= mi
    intersections il = case il of
      [] -> []
      tr : trs -> zip xs ys <> intersections trs
        where
          xs = mapMaybe (lineIntersection tr) trs
          ys = lineAt tr <$> xs
    p2 = h
    p1 = length $ filter inlimits $ intersections ts

main :: IO ()
main = readFile "input/day24.txt" >>= print . solve 200000000000000 400000000000000 . parse
