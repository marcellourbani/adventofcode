#!/usr/bin/env stack
-- stack --resolver lts-18.18 script --optimize

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as S

type Vector = (Int, Int)

data GameMap = GameMap {gW :: Int, gH :: Int, gMap :: M.Map Vector Char} deriving (Eq)

data State = State {sPos :: Vector, sSpeed :: Vector, sLen :: Int} deriving (Show, Eq, Ord)

mapTile :: GameMap -> Vector -> Int
mapTile gm p = read $ (: "") <$> fromMaybe '0' $ M.lookup p $ gMap gm

instance Show GameMap where
  show gm@(GameMap w h m) = unlines [[head $ show (mapTile gm (x, y)) | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]

parse :: String -> GameMap
parse s = GameMap (length $ head ls) (length ls) m
  where
    m = M.fromList [((x, y), c) | (y, l) <- zip [0 ..] ls, (x, c) <- zip [0 ..] l]
    ls = lines s

inMap :: GameMap -> Vector -> Bool
inMap (GameMap w h _) (x, y) = x >= 0 && x < w && y >= 0 && y < h

addVector :: Vector -> Vector -> Vector
addVector (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

nextStates :: GameMap -> State -> [State]
nextStates gm s@(State p v l) = filter (inMap gm . sPos) $ mapMaybe ns vs
  where
    vs = ((0,) <$> [1, -1]) <> ((,0) <$> [1, -1])
    ns v1@(x, y)
      | (-x, -y) == v = Nothing
      | v1 == v && l == 3 = Nothing
      | v1 == v = Just $ State (addVector p v) v $ l + 1
      | otherwise = Just $ State (addVector p v1) v1 1

minimumPath :: GameMap -> State -> Int
minimumPath gamemap@(GameMap w h _) initial = minimum $ M.filterWithKey isBr final
  where
    stateV s = mapTile gamemap $ sPos s
    nexts (s, v) = zip ns $ (v +) . stateV <$> ns where ns = nextStates gamemap s
    final = go [(initial, 0)] M.empty
    isBr s _ = (w - 1, h - 1) == sPos s
    go edge scores
      | null edge = scores
      | otherwise = go edge' scores'
      where
        nextStates = S.toList $ S.fromList $ edge >>= nexts
        edge' = mapMaybe improved nextStates
        improved (s, v) = case scores M.!? s of
          Nothing -> Just (s, v)
          Just v' | v < v' -> Just (s, v)
          _ -> Nothing
        scores' = M.union (M.fromList edge') scores

-- >>> solve $ parse "2413432311323\n3215453535623\n3255245654254\n3446585845452\n4546657867536\n1438598798454\n4457876987766\n3637877979653\n4654967986887\n4564679986453\n1224686865563\n2546548887735\n4322674655533"
-- 134

solve l = p1
  where
    p1 = minimumPath l $ State (0, 0) (0, 0) 0

main :: IO ()
main = readFile "input/day17.txt" >>= print . solve . parse