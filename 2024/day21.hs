#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

{-# LANGUAGE PackageImports #-}

module Main where

import Control.Lens ((^.))
import Data.List (filter, nub)
import Data.Maybe (fromMaybe)
import Data.PQueue.Prio.Min qualified as P
import Linear.V2
import "containers" Data.Map.Strict qualified as M

type Pad = M.Map

data GameMap c = GameMap {gmW :: Int, gmH :: Int, gmMap :: M.Map (V2 Int) c} deriving (Eq)

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

drawPath :: Bool -> GameMap c -> M.Map (V2 Int) c -> GameMap c
drawPath drawover gm m = gm {gmMap = if drawover then M.union m (gmMap gm) else M.union (gmMap gm) m}

keysOf :: (Eq c) => GameMap c -> c -> [V2 Int]
keysOf gm g = M.keys $ M.filter (== g) $ gmMap gm

parseGm :: Char -> String -> GameMap Char
parseGm nc s = GameMap w h $ M.fromList [(V2 x y, c) | (y, line) <- zip [0 ..] l, (x, c) <- zip [0 ..] line, c /= nc]
  where
    l = lines s
    w = length $ head l
    h = length l

fromMap :: M.Map (V2 Int) c -> GameMap c
fromMap m = GameMap w h m
  where
    w = 1 + maximum ((^. _x) <$> M.keys m)
    h = 1 + maximum ((^. _y) <$> M.keys m)

parse :: String -> [String]
parse = lines

parsePad :: String -> GameMap Char
parsePad s = fromMap gm'
  where
    base@(GameMap w h gm) = parseGm ' ' s
    cvc o d a = (a - o) `div` d
    ks = M.toList $ M.filter (`notElem` "+|-") gm
    gm' = M.fromList [(V2 (cvc 2 4 x) (cvc 1 2 y), k) | (V2 x y, k) <- ks]

numKeypad :: GameMap Char
numKeypad = parsePad "+---+---+---+\n| 7 | 8 | 9 |\n+---+---+---+\n| 4 | 5 | 6 |\n+---+---+---+\n| 1 | 2 | 3 |\n+---+---+---+\n    | 0 | A |\n    +---+---+"

dirKeypad :: GameMap Char
dirKeypad = parsePad "    +---+---+\n    | ^ | A |\n+---+---+---+\n| < | v | > |\n+---+---+---+"

driveKeypad :: GameMap Char -> Char -> Char -> [String]
driveKeypad nk f t = paths
  where
    (x1, y1, x2, y2) = case (keysOf nk f, keysOf nk t) of
      ([V2 x1 y1], [V2 x2 y2]) -> (x1, y1, x2, y2)
    xs = if x2 > x1 then '>' <$ [x1 .. x2 - 1] else '<' <$ [x2 .. x1 - 1]
    ys = if y2 > y1 then 'v' <$ [y1 .. y2 - 1] else '^' <$ [y2 .. y1 - 1]
    part a = splitAt <$> [0 .. length a] <*> [a]
    paths = nub [xa <> ya <> xb <> yb | (xa, xb) <- part xs, (ya, yb) <- part ys]

typeSeq :: GameMap Char -> [Char] -> [[Char]]
typeSeq gm s = go $ 'A' : s
  where
    go p = case p of
      [] -> []
      [_] -> [""]
      f : t : r -> [pf <> ('A' : re) | pf <- driveKeypad gm f t, re <- go (t : r)]

a = head $ typeSeq dirKeypad $ head $ typeSeq dirKeypad $ head (typeSeq numKeypad "029A")

part1 :: [String] -> [(Int, Int)]
part1 l = score <$> l
  where
    score x = (val x, minl x)
    minl x = length $ head $ typeSeq dirKeypad $ head $ typeSeq dirKeypad $ head (typeSeq numKeypad x)
    val x = read (filter (/= 'A') x)

part2 l = 0

-- >>> solve $ parse "029A\n980A\n179A\n456A\n379A"
-- ([(29,68),(980,60),(179,64),(456,60),(379,64)],0)
-- 68 * 29, 60 * 980, 68 * 179, 64 * 456, and 64 * 379.

solve l = (p1, p2)
  where
    p1 = part1 l
    p2 = part2 l

main :: IO ()
main = readFile "input/day21.txt" >>= print . solve . parse
