#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S

data ElfMap = ElfMap {unElfMap :: S.Set (Int, Int), moves :: [(Int, Int)]} deriving (Eq)

instance Show ElfMap where
  show (ElfMap s ms) =
    "From " <> show (minx, miny) <> " to " <> show (maxx, maxy)
      <> "\n"
      <> unlines (l <$> [miny .. maxy])
      <> show ms
      <> " \n \n"
    where
      ks = S.toList s
      minx = minimum $ fst <$> ks
      miny = minimum $ snd <$> ks
      maxx = maximum $ fst <$> ks
      maxy = maximum $ snd <$> ks
      l y = [c | x <- [minx .. maxx], let c = if S.member (x, y) s then '#' else '.']
      m = [(0, -1), (0, 1), (1, 0), (-1, 0)]

parse :: String -> ElfMap
parse s = ElfMap se [(0, -1), (0, 1), (-1, 0), (1, 0)]
  where
    se = S.fromList [(x, y) | (y, l) <- zip [0 ..] (lines s), (x, c) <- zip [0 ..] l, c == '#']

-- moves :: [(Int, Int)]
-- moves = [(0, -1), (0, 1), (1, 0), (-1, 0)]

addVec :: (Int, Int) -> (Int, Int) -> (Int, Int)
addVec (x, y) (mx, my) = (x + mx, y + my)

neighbors :: (Int, Int) -> (Int, Int) -> S.Set (Int, Int)
neighbors (x, y) (mx, my) = S.fromList $ case mx of
  0 -> zip [x -1 .. x + 1] $ repeat $ y + my
  _ -> zip (repeat $ x + mx) [y -1 .. y + 1]

advanceMap :: ElfMap -> ElfMap
advanceMap e@(ElfMap em moves)
  | null movableelves = e
  | otherwise = ElfMap em' moves'
  where
    movableelves = filter movable $ S.toList em
    posMoves = catMaybes $ pickMove <$> movableelves <*> [moves]
    destinations = M.unionsWith (+) $ M.singleton . uncurry addVec <$> posMoves <*> [1]
    realMoves = M.fromList $ filter single posMoves
    em' = S.map moveElf em
    movable (x, y) = or [S.member (a, b) em | a <- [x -1 .. x + 1], b <- [y -1 .. y + 1], (a, b) /= (x, y)]
    moves' = tail moves <> [head moves]
    moveElf e = case M.lookup e realMoves of
      Nothing -> e
      Just m -> addVec e m
    single (e, d) = M.lookup (addVec e d) destinations == Just 1
    pickMove e m = case m of
      [] -> Nothing
      m1 : ms | S.null $ S.intersection em (neighbors e m1) -> Just (e, m1)
      m1 : ms -> pickMove e ms

-- >>> solve $ parse "..............\n..............\n.......#......\n.....###.#....\n...#...#.#....\n....#...##....\n...#.###......\n...##.#.##....\n....#..#......\n..............\n..............\n.............."
-- (110,20)

solve :: ElfMap -> (Int, Int)
solve l = (p1, p2)
  where
    p2 = go l 0
    go s n
      | s' == s = n + 1
      | otherwise = go s' $ n + 1
      where
        s' = advanceMap s
    p1 = emptyCells $ iterate advanceMap l !! 10
    emptyCells (ElfMap em _) = ((maximum xs - minimum xs + 1) * (maximum ys - minimum ys + 1)) - S.size em
      where
        xs = fst <$> S.toList em
        ys = snd <$> S.toList em

main :: IO ()
main = readFile "input/day23.txt" >>= print . solve . parse
