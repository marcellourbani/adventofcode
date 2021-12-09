#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S

data Input = Input {maxx :: Int, maxy :: Int, values :: M.Map (Int, Int) Int} deriving (Show)

parse :: String -> Input
parse s = Input maxx maxy $ M.fromList points
  where
    m = map (read . pure) <$> lines s
    points = [((x, y), v) | (y, l) <- zip [0 ..] m, (x, v) <- zip [0 ..] l]
    maxx = maximum $ fst . fst <$> points
    maxy = maximum $ snd . fst <$> points

-- basin :: (Int, Int) -> Input -> S.Set (Int, Int)
basin sp (Input maxx maxy i) = go (S.singleton sp)
  where
    is9 p = case (== 9) <$> i M.!? p of
      Just True -> Just p
      _ -> Nothing
    go s = if S.empty == toAdd then s else go (S.union s toAdd)
      where
        newxs = S.unions $ S.fromList . hneigh s <$> S.toList s
        newys = S.unions $ S.fromList . vneigh s <$> S.toList s
        toAdd = S.union newxs newys

    hneigh s (x, y) = filter (`S.notMember` s) $ (,y) <$> [leftb .. rightb]
      where
        leftb = case catMaybes $ is9 . (,y) <$> [x, x -1 .. 0] of
          p : _ -> fst p + 1
          _ -> 0
        rightb = case catMaybes $ is9 . (,y) <$> [x .. maxx] of
          p : _ -> fst p - 1
          _ -> maxx

    vneigh s (x, y) = filter (`S.notMember` s) $ (x,) <$> [topb .. bottomb]
      where
        topb = case catMaybes $ is9 . (,y) <$> [y, y -1 .. 0] of
          p : _ -> snd p + 1
          _ -> 0
        bottomb = case catMaybes $ is9 . (,y) <$> [y .. maxy] of
          p : _ -> fst p -1
          _ -> maxx

-- >>> basin (1,0) $ parse "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"
-- fromList [(0,0),(0,1),(1,0),(1,1)]

-- >>> solve $ parse "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"
-- (15,0)

solve :: Input -> (Int, Int)
solve (Input _ _ i) = (sum risks, 0)
  where
    adjscoord (x, y) = ((,y) <$> [x -1, x + 1]) <> ((x,) <$> [y - 1, y + 1])
    adjacents p = catMaybes $ (M.!?) i <$> adjscoord p
    isMin (c, v) = all (> v) $ adjacents c
    lowPoints = filter isMin $ M.toList i
    risks = (+ 1) . snd <$> lowPoints

main :: IO ()
main = readFile "input/day9.txt" >>= print . solve . parse
