#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

type Vector = (Int, Int)

data GameMap = GameMap {gW :: Int, gH :: Int, gMap :: M.Map Vector Char} deriving (Eq)

data Ray = Ray {rPos :: Vector, rSpeed :: Vector} deriving (Show, Eq)

mapTile :: GameMap -> Vector -> Char
mapTile gm p = fromMaybe '.' $ M.lookup p $ gMap gm

instance Show GameMap where
  show gm@(GameMap w h m) = unlines [[mapTile gm (x, y) | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]

parse :: String -> GameMap
parse s = GameMap (length $ head ls) (length ls) m
  where
    m = M.fromList [((x, y), c) | (y, l) <- zip [0 ..] ls, (x, c) <- zip [0 ..] l]
    ls = lines s

nextRay :: Ray -> Ray
nextRay (Ray (x, y) (dx, dy)) = Ray (x + dx, y + dy) (dx, dy)

inMap :: GameMap -> Ray -> Bool
inMap (GameMap w h _) (Ray (x, y) _) = x >= 0 && x < w && y >= 0 && y < h

collide :: Ray -> Char -> [Ray]
collide r@(Ray (x, y) (dx, dy)) c = case c of
  '\\' | dy == 0 -> [rr]
  '\\' -> [rl]
  '/' | dy == 0 -> [rl]
  '/' -> [rr]
  '|' | dy == 0 -> [rl, rr]
  '-' | dx == 0 -> [rl, rr]
  _ -> [r]
  where
    rr = Ray (x - dy, y + dx) (-dy, dx)
    rl = Ray (x + dy, y - dx) (dy, -dx)

rayArrow :: Ray -> Char
rayArrow (Ray _ (x, y)) = case (x, y) of
  (1, 0) -> '>'
  (-1, 0) -> '<'
  (0, 1) -> 'v'
  (0, -1) -> '^'

walkGameMap :: GameMap -> Ray -> M.Map Vector [Char]
walkGameMap gm ray = go gm [ray] $ rmap [ray]
  where
    rmap rs = M.fromList $ zip (rPos <$> rs) ((: []) . rayArrow <$> rs)
    nr r@(Ray p v) = filter (inMap gm) $ collide r $ mapTile gm p
    nr' r = case nr r of
      [] -> []
      [r'] | r' == r -> filter (inMap gm) [nextRay r]
      rs -> filter (inMap gm) rs
    go gmap rs visited
      | null newRays = visited
      | otherwise = go gmap newRays visited'
      where
        newD r = rayArrow r `notElem` M.findWithDefault [] (rPos r) visited
        newRays = filter newD $ rs >>= nr'
        newVisited = rmap newRays
        visited' = M.unionWith (++) visited newVisited

evolve :: GameMap -> Ray -> GameMap
evolve gm rs = gm {gMap = M.union visited $ gMap gm}
  where
    visited = toC <$> walkGameMap gm rs
    toC l = case length l of
      0 -> '.'
      1 -> head l
      _ -> head $ show $ length l

-- >>> solve $ parse ".|...\\....\n|.-.\\.....\n.....|-...\n........|.\n..........\n.........\\\n..../.\\\\..\n.-.-/..|..\n.|....-|.\\\n..//.|...."
-- (46,51)

solve l@(GameMap w h gm) = (p1, p2)
  where
    initial = Ray (0, 0) (1, 0)
    rc ti l v = Ray . ti <$> [0 .. l - 1] <*> [v]
    rawcandidateStarts = rc (0,) h (1, 0) <> rc (w - 1,) h (-1, 0) <> rc (,0) w (0, 1) <> rc (,h - 1) w (0, -1)
    valid c = case collide c $ mapTile l (rPos c) of
      [c'] -> c' == c
      _ -> False
    candidateStarts = filter valid rawcandidateStarts
    active p = length $ walkGameMap l p
    p1 = active initial
    p2 = maximum $ active <$> candidateStarts

main :: IO ()
main = readFile "input/day16.txt" >>= print . solve . parse
