#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import Control.Arrow (first, second)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Tile = Tile {tWidth :: Int, tHeight :: Int, tDots :: S.Set (Int, Int)} deriving (Eq, Ord)

type Transform = M.Map Tile Tile

instance Show Tile where
  show (Tile w h s) = unlines [s | y <- [0 .. h - 1], let s = [ch (x, y) | x <- [0 .. w - 1]]]
    where
      ch c = if S.member c s then '#' else '.'

parse :: String -> Transform
parse s = M.fromList $ parseLine <$> lines s
  where
    parseLine l = (a, b) where [a, b] = toTile . splitOn "/" <$> splitOn " => " l

toTile :: [String] -> Tile
toTile g = Tile (length $ head g) (length g) $ S.fromList [(x, y) | (y, l) <- zip [0 ..] g, (x, c) <- zip [0 ..] l, c == '#']

rotateRight :: Tile -> Tile
rotateRight (Tile w h s) = Tile h w $ S.fromList $ rr <$> S.toList s
  where
    rr (x, y) = (h - 1 - y, x)

flipH :: Tile -> Tile
flipH (Tile w h s) = Tile w h $ S.fromList $ fh <$> S.toList s
  where
    fh (x, y) = (w - 1 - x, y)

splitTile :: Tile -> [[Tile]]
splitTile (Tile w h s)
  | even w = splitBy 2
  | mod w 3 == 0 = splitBy 3
  | otherwise = undefined
  where
    byRange n x1 y1 (x, y) = x >= x1 && x < x1 + n && y >= y1 && y < y1 + n
    translate x1 y1 (x, y) = (x - x1, y - y1)
    translateSet x1 y1 s = S.fromList $ translate x1 y1 <$> S.toList s
    filtered n x1 y1 = Tile n n $ translateSet x1 y1 $ S.filter (byRange n x1 y1) s
    splitBy n = [t | y <- [0, n .. h - 1], let t = [filtered n x y | x <- [0, n .. w - 1]]]

mergeTiles :: [[Tile]] -> Tile
mergeTiles t = mergeVert $ mergeLine <$> t
  where
    translateH w s = S.fromList $ first (+ w) <$> S.toList s
    translateV h s = S.fromList $ second (+ h) <$> S.toList s
    mergeVert l = case l of
      [a] -> a
      (Tile w h1 s1) : (Tile _ h2 s2) : ts -> mergeVert $ merged : ts
        where
          merged = Tile w (h1 + h2) (S.union s1 $ translateV h1 s2) -- assume same w
    mergeLine l = case l of
      [a] -> a
      (Tile w1 h s1) : (Tile w2 _ s2) : ts -> mergeLine $ merged : ts
        where
          merged = Tile (w1 + w2) h (S.union s1 $ translateH w1 s2) -- assume same h

transformPart :: Transform -> Tile -> Tile
transformPart tm t@(Tile w h s) = case M.lookup t tm of
  Just t'' -> t''
  Nothing -> Tile (w + 1) (h + 1) s

transformTile :: Transform -> Tile -> Tile
transformTile tm tile = mergeTiles decoded
  where
    parts = splitTile tile
    decoded = map (transformPart tm) <$> parts

addTransformed :: Transform -> Transform
addTransformed tr = M.fromList $ M.toList tr >>= variants
  where
    rots ti = take 4 $ iterate rotateRight ti
    variants (k, v) = (,v) <$> (rots k <> (flipH <$> rots k))

seed :: Tile
seed = toTile $ lines ".#.\n..#\n###"

-- >>> solve $ parse "../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#"
-- (22,22)

-- brute force, p2 takes over 3 hours, does give the right answer eventually
solve :: Transform -> (Int, Int)
solve t = (p1, p2)
  where
    allTr = addTransformed t
    p1 = S.size $ tDots $ ts !! 5
    p2 = S.size $ tDots $ ts !! 18
    ts = iterate (transformTile allTr) seed

main :: IO ()
main = readFile "input/day21.txt" >>= print . solve . parse
