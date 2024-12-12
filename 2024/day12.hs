#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

{-# LANGUAGE PackageImports #-}

module Main where

import Control.Lens ((^.))
import Data.Maybe (fromMaybe)
import Data.PQueue.Prio.Min qualified as P
import Distribution.Simple.Utils (debug)
import Linear.V2
import "containers" Data.Map.Strict qualified as M
import "containers" Data.Set qualified as S

data GameMap c = GameMap {gmW :: Int, gmH :: Int, gmMap :: M.Map (V2 Int) c} deriving (Eq)

data Region = Region {rC :: Char, rS :: S.Set (V2 Int)} deriving (Show)

instance Functor GameMap where
  fmap f (GameMap w h m) = GameMap w h $ M.map f m

instance Show (GameMap Char) where
  show gm@(GameMap w h m) = unlines [[mapTile '.' gm (V2 x y) | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]

instance {-# OVERLAPPABLE #-} (Show c) => Show (GameMap c) where
  show gm@(GameMap w h m) = show $ gm {gmMap = head . show <$> m}

mapTile :: c -> GameMap c -> V2 Int -> c
mapTile def gm p = fromMaybe def $ M.lookup p $ gmMap gm

inMap :: GameMap c -> V2 Int -> Bool
inMap (GameMap w h _) (V2 x y) = 0 <= x && x < w && 0 <= y && y < h

parse :: String -> GameMap Char
parse s = GameMap w h $ M.fromList [(V2 x y, c) | (y, line) <- zip [0 ..] l, (x, c) <- zip [0 ..] line, c /= '.']
  where
    l = lines s
    w = length $ head l
    h = length l

type Input = GameMap Char

directions :: [V2 Int]
directions = [V2 0, flip V2 0] <*> [-1, 1]

perimeter :: Region -> Int
perimeter (Region _ s) = sum $ per <$> S.toList s
  where
    per l = length $ filter (`S.notMember` s) $ (+ l) <$> directions

sides :: Region -> Int
sides (Region _ s) = length horizontals + length verticals
  where
    xs = (^. _x) <$> S.toList s
    ys = (^. _y) <$> S.toList s
    (minx, maxx, miny, maxy) = (minimum xs, maximum xs, minimum ys, maximum ys)
    verticals = collectEdges S.empty S.empty vertedges
    vertedges = S.fromList . edges . column <$> [minx .. maxx]
    horizontals = collectEdges S.empty S.empty horedges
    horedges = S.fromList . edges . line <$> [miny .. maxy]

    m x y = S.member (V2 x y) s
    line y = [(m x y, x, y) | x <- [minx - 1 .. maxx + 1]]

    column x = [(m x y, y, x) | y <- [miny - 1 .. maxy + 1]]
    collectEdges acc p l = case l of
      [] -> acc
      x : xs -> collectEdges acc' x xs
        where
          ign (a, b, _) = (a, b)
          toIgn = flip S.notMember (S.map ign p) . ign
          toadd = S.filter toIgn x --  S.difference x p
          acc' = S.union acc toadd
    edges l = case l of
      a@(False, _, _) : b@(True, _, _) : ts -> a : edges (b : ts) -- enter
      a@(True, _, _) : b@(False, _, _) : ts -> a : edges (b : ts) -- exit
      (True, _, _) : ts -> edges ts -- no transition
      (False, _, _) : ts -> edges ts -- no transition
      _ -> []

cost :: Region -> Int
cost r@(Region _ s) = perimeter r * S.size s

cost2 :: Region -> Int
cost2 r@(Region _ s) = sides r * S.size s

region :: Input -> V2 Int -> Region
region gm seed = Region c $ go (S.singleton seed) [seed]
  where
    mt = mapTile ' ' gm
    c = mt seed
    valid kn p = c /= ' ' && S.notMember p kn && mt p == c
    go acc fr
      | null fr' = acc
      | otherwise = go acc' fr''
      where
        fr' = S.fromList $ filter (valid acc) $ (+) <$> fr <*> directions
        fr'' = S.toList fr'
        acc' = S.union acc fr'

regions :: Input -> [Region]
regions i@(GameMap _ _ !gm)
  | M.null gm = []
  | otherwise = r : regions i'
  where
    seed = head $ M.keys gm
    r = region i seed
    notR k _ = S.notMember k (rS r)
    i' = i {gmMap = M.filterWithKey notR gm}

part1 :: [Region] -> Int
part1 l = sum $ cost <$> l

-- part2 :: Input -> Int
part2 :: [Region] -> Int
part2 l = sum $ cost2 <$> l

-- >>> solve $ parse "AAAA\nBBCD\nBBCC\nEEEC"
-- >>> solve $ parse "OOOOO\nOXOXO\nOOOOO\nOXOXO\nOOOOO"
-- >>> solve $ parse "EEEEE\nEXXXX\nEEEEE\nEXXXX\nEEEEE"
-- >>> solve $ parse "RRRRIICCFF\nRRRRIICCCF\nVVRRRCCFFF\nVVRCCCJFFF\nVVVVCJJCFE\nVVIVCCJJEE\nVVIIICJJEE\nMIIIIIJJEE\nMIIISIJEEE\nMMMISSJEEE"
-- (140,80)
-- (772,436)
-- (692,236)
-- (1930,1206)

solve :: Input -> (Int, Int)
solve l = (p1, p2)
  where
    rs = regions l
    p1 = part1 rs
    p2 = part2 rs

main :: IO ()
main = readFile "input/day12.txt" >>= print . solve . parse
