#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import Data.Bits (xor)
import Data.Foldable (Foldable (foldr'))
import Data.List (partition, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Numeric (readHex)

data Direction = R | D | L | U deriving (Show, Eq, Read, Enum)

data Instruction = Instruction {iDir :: Direction, iLen :: Int, iColour :: Int} deriving (Show, Eq)

newtype Vector = Vector {unVector :: (Int, Int)} deriving (Show, Eq)

data Segment = Segment {sStart :: Vector, sVertical :: Bool, sLen :: Int} deriving (Show, Eq)

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

convertInstruction :: Instruction -> Instruction
convertInstruction (Instruction d n c) = Instruction (toEnum $ mod c 16) (div c 16) c

follow :: [Instruction] -> [Segment]
follow = go (Vector (0, 0))
  where
    toSegment v@(Vector (x, y)) (Instruction d n _) = case d of
      R -> (Segment v False n, Vector (x + n, y))
      D -> (Segment v True n, Vector (x, y + n))
      L -> (Segment (Vector (x - n, y)) False n, Vector (x - n, y))
      U -> (Segment (Vector (x, y - n)) True n, Vector (x, y - n))
    go v l = case l of
      [] -> []
      i : is -> s : go v' is
        where
          (s, v') = toSegment v i

dig2 :: [Segment] -> Int
dig2 segs = go $ S.toAscList $ S.map snd junctions
  where
    verticals = filter sVertical segs
    sjunctions (Segment (Vector (x, y)) v l) = if v then [(x, y), (x, y + l)] else []
    vstarts = S.fromList $ unVector . sStart <$> verticals
    crosses x1 x2 y = S.member (x1, y) vstarts `xor` S.member (x2, y) vstarts
    junctions = S.fromList $ verticals >>= sjunctions
    ony y (Segment (Vector (px, py)) v l) = [(px, y == py || y == py + l) | v && py <= y && y <= py + l]
    ls y lastx l = case (lastx, l) of
      (Nothing, []) -> 0
      (Nothing, [_]) -> 1
      (Nothing, (x1, False) : xs) -> ls y (Just x1) xs
      (Just lx, (x1, False) : xs) -> x1 - lx + 1 + ls y Nothing xs
      (Nothing, (x1, True) : (x2, True) : xs)
        | crosses x1 x2 y -> x2 - x1 + ls y (Just x2) xs
        | otherwise -> x2 - x1 + 1 + ls y Nothing xs
      (Just lx, (x1, True) : (x2, True) : xs)
        | crosses x1 x2 y -> x2 - lx + 1 + ls y Nothing xs
        | otherwise -> ls y lastx xs

    ylinesize y = ls y Nothing $ sort $ verticals >>= ony y
    go l = case l of
      [] -> 0
      [y] -> ylinesize y
      y1 : y2 : ys | y2 == y1 + 1 -> ylinesize y1 + go (y2 : ys)
      y1 : y2 : ys -> ylinesize y1 + (y2 - y1 - 1) * ylinesize (y1 + 1) + go (y2 : ys)

-- >>> solve $ parse "R 6 (#70c710)\nD 5 (#0dc571)\nL 2 (#5713f0)\nD 2 (#d2c081)\nR 2 (#59c680)\nD 2 (#411b91)\nL 5 (#8ceee2)\nU 2 (#caa173)\nL 1 (#1b58a2)\nU 2 (#caa171)\nR 2 (#7807d2)\nU 3 (#a77fa3)\nL 2 (#015232)\nU 2 (#7a21e3)"
-- (62,952408144115)
solve l = (p1, p2)
  where
    p1 = dig2 $ follow l
    p2 = dig2 $ follow $ convertInstruction <$> l

main :: IO ()
main = readFile "input/day18.txt" >>= print . solve . parse
