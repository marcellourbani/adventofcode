#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (Foldable (foldl'))
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

data Direction = TR | TL | Advance Int deriving (Show)

data Facing = R | D | L | U deriving (Show, Enum)

data State = State {sCoord :: (Int, Int), sFacing :: Facing} deriving (Show)

newtype GameMap = GameMap {unGm :: M.Map (Int, Int) Bool}

instance Show GameMap where
  show (GameMap m) = show (maxx + 1, maxy + 1) <> "\n" <> unlines (lin <$> [miny .. maxy])
    where
      lim vs = (minimum vs, maximum vs)
      lin y = [toc $ M.lookup k m | k <- zip [minx .. maxx] $repeat y]
      toc v = case v of
        Just True -> '#'
        Just False -> '.'
        Nothing -> ' '
      (minx, maxx) = lim $ fst <$> M.keys m
      (miny, maxy) = lim $ snd <$> M.keys m

parse :: String -> (GameMap, [Direction])
parse s = (pm rm, pd rawdir "")
  where
    [rawmap, rawdir] = splitOn "\n\n" s
    rm = zip [0 ..] $zip [0 ..] <$> lines rawmap
    pm m = GameMap $ M.fromList [((x, y), v) | (y, l) <- rm, (x, c) <- l, let v = c == '#', c == '.' || v]
    pd rem n = case rem of
      [] -> [Advance $ read n]
      'L' : rs -> Advance (read n) : TL : pd rs ""
      'R' : rs -> Advance (read n) : TR : pd rs ""
      r : rs -> pd rs $ n <> [r]

advance :: GameMap -> (Int, Int) -> Facing -> (Int, Int)
advance (GameMap m) c@(x, y) d = case M.lookup (x'', y'') m of
  Just True -> c
  Just False -> (x'', y'')
  _ -> undefined
  where
    x' = case d of
      R -> x + 1
      L -> x - 1
      _ -> x
    y' = case d of
      D -> y + 1
      U -> y - 1
      _ -> y
    topc a = minimum $ snd <$> filter ((== a) . fst) (M.keys m)
    botc a = maximum $ snd <$> filter ((== a) . fst) (M.keys m)
    leftc a = minimum $ fst <$> filter ((== a) . snd) (M.keys m)
    rightc a = maximum $ fst <$> filter ((== a) . snd) (M.keys m)
    ml = M.lookup (x', y') m
    y'' = case (d, ml) of
      (D, Nothing) -> topc x'
      (U, Nothing) -> botc x'
      (_, Just True) -> y
      _ -> y'
    x'' = case (d, ml) of
      (R, Nothing) -> leftc y'
      (L, Nothing) -> rightc y'
      (_, Just True) -> x
      _ -> x'

followDirection :: GameMap -> State -> Direction -> State
followDirection gm@(GameMap m) s@(State (x, y) f) dir = case dir of
  TL -> s {sFacing = toEnum $ mod (fromEnum f - 1) 4}
  TR -> s {sFacing = toEnum $ mod (fromEnum f + 1) 4}
  Advance n
    | n == 0 -> s
    | otherwise -> followDirection gm s' (Advance $n -1)
    where
      s' = s {sCoord = advance gm (x, y) f}

-- >>> solve $ parse "        ...#\n        .#..\n        #...\n        ....\n...#.......#\n........#...\n..#....#....\n..........#.\n        ...#....\n        .....#..\n        .#......\n        ......#.\n\n10R5L5R10L4R5L5"
-- 6032
solve (gm, dir) = p1
  where
    x = minimum $ fst <$> filter ((== 0) . snd) (M.keys $unGm gm)
    initial = State (x, 0) R
    p1 = score $ foldl' (followDirection gm) initial dir
    score (State (fx, fy) fh) = 1000 * (fy + 1) + 4 * (fx + 1) + fromEnum fh

main :: IO ()
main = readFile "input/day22.txt" >>= print . solve . parse
