#!/usr/bin/env stack
-- stack --resolver lts-18.18 script --optimize

{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import Data.Foldable (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, isNothing)
import qualified Data.Set as S

type Vector = (Int, Int)

data GameMap = GameMap {gWidth :: Int, gHeight :: Int, gRounds :: S.Set Vector, gSquares :: S.Set Vector} deriving (Eq)

instance Show GameMap where
  show (GameMap w h ro sq) = unlines $ line <$> [0 .. h - 1]
    where
      line y = [c (x, y) | x <- [0 .. w - 1]]
      c p
        | S.member p ro = 'O'
        | S.member p sq = '#'
        | otherwise = '.'

parse :: String -> GameMap
parse s = GameMap w h (bychar 'O') (bychar '#')
  where
    bychar c = S.fromList [(x, y) | (y, l) <- zip [0 ..] ls, (x, ch) <- zip [0 ..] l, ch == c]
    ls = lines s
    w = length $ head ls
    h = length ls

moveUntilPossible :: GameMap -> Vector -> Vector -> Vector
moveUntilPossible (GameMap w h ro sq) (vx, vy) = go
  where
    next (a, b) = (a + vx, b + vy)
    free v = not $ S.member v ro || S.member v sq
    valid (a, b) = a >= 0 && a < w && b >= 0 && b < h && free (a, b)
    go v = if valid v' then go v' else v where v' = next v

moveGameMap :: GameMap -> Vector -> GameMap
moveGameMap gm@(GameMap _ _ ro _) v
  | ro' == ro = gm
  | otherwise = moveGameMap (gm {gRounds = ro'}) v
  where
    ro' = S.map (moveUntilPossible gm v) ro

cycleGm :: GameMap -> GameMap
cycleGm gm = foldl' moveGameMap gm [(0, -1), (-1, 0), (0, 1), (1, 0)]

weights :: GameMap -> [Int]
weights (GameMap _ h r _) = (h -) . snd <$> S.toList r

-- runCycles :: GameMap -> Int -> GameMap
runCycles gm n = go initial gm 0
  where
    initial = M.singleton (gRounds gm) 0
    go cache gmap num = case ro'' of
      _ | num == n -> gmap
      Nothing -> go cache' gmap' num'
      Just r -> gmap {gRounds = r}
      where
        gmap'@(GameMap _ _ ro _) = cycleGm gmap
        num' = num + 1
        ro' = gRounds $ cycleGm gmap
        cached = M.lookup ro' cache
        nextidx i = i + mod (n - num') (num' - i)
        nextr i = case M.toList $ M.filter (== nextidx i) cache of
          [] -> Nothing
          (r, _) : _ -> Just r
        ro'' = cached >>= nextr
        cache' = M.insert ro' num' cache

-- >>> solve $ parse "O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#...."
-- (136,64)

solve :: GameMap -> (Int, Int)
solve l = (p1, p2)
  where
    p1 = sum $ weights $ moveGameMap l (0, -1)
    p2 = sum $ weights $ runCycles l 1000000000

main :: IO ()
main = readFile "input/day14.txt" >>= print . solve . parse
