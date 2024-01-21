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

minPath :: GameMap -> Int
minPath gamemap@(GameMap w h _) = go 0 initial initial
  where
    initial = M.singleton (State (0, 0) (0, 0) 0) 0
    isFinal s = sPos s == (w - 1, h - 1)
    findFinal s = S.filter isFinal $ M.keysSet s
    addValue c p = (p, c + mapTile gamemap (sPos p))
    go n costs frontier =
      if M.null frontier
        then minimum $ M.restrictKeys costs $ findFinal costs
        else go (n + 1) costs' frontier'
      where
        improved k v = M.findWithDefault maxBound k costs > v
        nexts (p, c) = M.fromList $ addValue c <$> nextStates gamemap p
        newCandidates = M.unionsWith min $ nexts <$> M.toList frontier
        frontier' = M.filterWithKey improved newCandidates
        costs' = M.unionWith min costs frontier'

solve :: GameMap -> Int
solve l = p1
  where
    p1 = minPath l

main :: IO ()
main = readFile "input/day17.txt" >>= print . solve . parse