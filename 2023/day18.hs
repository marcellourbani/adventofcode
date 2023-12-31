#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import Data.Bits (xor)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Numeric (readHex)

data Direction = U | D | L | R deriving (Show, Eq, Read)

data Instruction = Instruction {iDir :: Direction, iLen :: Int, iColour :: Int} deriving (Show, Eq)

newtype Vector = Vector {unVector :: (Int, Int)} deriving (Show, Eq)

instance Num Vector where
  (+) (Vector (a, b)) (Vector (c, d)) = Vector (a + c, b + d)
  (*) (Vector (a, b)) (Vector (c, d)) = Vector (a * c, b * d)
  abs (Vector (a, b)) = Vector (abs a, abs b)
  signum = undefined
  fromInteger a = Vector (fromInteger a, fromInteger a)
  negate (Vector (a, b)) = Vector (-a, -b)

data GameMap c = GameMap {gmW :: Int, gmH :: Int, gmMap :: M.Map (Int, Int) c} deriving (Eq)

instance Show (GameMap Char) where
  show gm@(GameMap w h m) = unlines [[mapTile '.' gm (x, y) | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]

instance Functor GameMap where
  fmap f (GameMap w h m) = GameMap w h $ M.map f m

parse :: String -> [Instruction]
parse s = parseLine . words <$> lines (filter (`notElem` "()#") s)
  where
    parseLine [a, b, c] = Instruction (read a) (read b) $ (fst . head . readHex) c

mapTile :: c -> GameMap c -> (Int, Int) -> c
mapTile def gm p = fromMaybe def $ M.lookup p $ gmMap gm

decodeDir :: Direction -> Vector
decodeDir d = Vector $ case d of
  U -> (0, -1)
  D -> (0, 1)
  L -> (-1, 0)
  R -> (1, 0)

tracePath :: Vector -> [Instruction] -> [(Vector, Direction)]
tracePath v p = case p of
  [] -> []
  (Instruction _ 0 _) : is -> tracePath v is
  (Instruction d n c) : is -> (v, d) : tracePath v' is'
    where
      v' = v + decodeDir d
      is' = Instruction d (n - 1) c : is

toGameMap :: [(Vector, Direction)] -> GameMap Direction
toGameMap l = GameMap w h $ M.fromList $ decode <$> l
  where
    points = unVector . fst <$> l
    (maxx, minx) = (maximum $ fst <$> points, minimum $ fst <$> points)
    (maxy, miny) = (maximum $ snd <$> points, minimum $ snd <$> points)
    decode (Vector (x, y), d) = ((x - minx, y - miny), d)

    w = maxx - minx + 1
    h = maxy - miny + 1

dig' :: [(Vector, Direction)] -> GameMap Char
dig' instructions = GameMap w h $ M.fromList $ [0 .. h] >>= filled
  where
    (GameMap w h m) = head . show <$> toGameMap instructions
    filled y = concat $ fillSegments False $ segments [] $ line y
    line y = filter ((== y) . snd . fst) $ M.toList m
    contiguous ((x1, _), _) ((x2, _), _) = x2 == x1 + 1
    segments seg l = case (seg, l) of
      ([], []) -> []
      (_, []) -> [reverse seg]
      ([], l1 : ls) -> segments [l1] ls
      (s1 : _, l1 : ls)
        | contiguous s1 l1 -> segments (l1 : seg) ls
        | otherwise -> reverse seg : segments [l1] ls
    lastx = fst . fst . last
    firstx = fst . fst . head
    isCrossing seg = case seg of
      [] -> False
      [_] -> True
      ((x1, y), _) : _ -> or $ cross <$> [(x1, x2), (x2, x1)]
        where
          x2 = lastx seg
          cross (a, b) = and $ M.member <$> [(a, y + 1), (b, y - 1)] <*> [m]
    fillSegments inside segs = case segs of
      [] -> []
      [s] -> [s]
      s1 : s2 : ss
        | inside' -> s1 : filler : fillSegments inside' segs'
        | otherwise -> s1 : fillSegments inside' segs'
        where
          inside' = xor inside $ isCrossing s1
          segs' = tail segs
          filler = [((x, snd . fst . head $ s1), '#') | x <- [lastx s1 + 1 .. firstx s2 - 1]]

-- >>> solve $ parse "R 6 (#70c710)\nD 5 (#0dc571)\nL 2 (#5713f0)\nD 2 (#d2c081)\nR 2 (#59c680)\nD 2 (#411b91)\nL 5 (#8ceee2)\nU 2 (#caa173)\nL 1 (#1b58a2)\nU 2 (#caa171)\nR 2 (#7807d2)\nU 3 (#a77fa3)\nL 2 (#015232)\nU 2 (#7a21e3)"
-- 62

solve l = p1
  where
    p1 = length . gmMap $ dig' $ tracePath (Vector (0, 0)) l

main :: IO ()
main = readFile "input/day18.txt" >>= print . solve . parse
