#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import qualified Data.Set as S

data Grid = Grid {gmaxx :: Int, gmaxy :: Int, gCells :: S.Set (Int, Int)} deriving (Eq)

instance Show Grid where
  show (Grid maxx maxy m) = "maxx: " <> show maxx <> " maxy: " <> show maxy <> "\n" <> grid <> "-- "
    where
      grid = unlines $ drawLine <$> [0 .. maxy]
      drawLine y = "-- " <> (toChar <$> linePoints)
        where
          linePoints = zip [0 .. maxx] $ repeat y
          toChar p = if S.member p m then '#' else '.'

parse :: String -> Grid
parse i = Grid (length (snd $head ls) -1) (length ls -1) $ S.fromList $ ls >>= pl
  where
    ls = zip [0 ..] $ lines i
    pl (y, l) = [(x, y) | (x, c) <- zip [0 ..] l, c == '#']

gridAt :: Grid -> (Int, Int) -> Bool
gridAt g (x, y) = S.member (x, y) $ gCells g

sumNeighbours :: Grid -> (Int, Int) -> Int
sumNeighbours g (x, y) = length $ filter (gridAt g) [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], dx /= 0 || dy /= 0]

next :: Grid -> Grid
next g@(Grid mx my _) = g {gCells = S.fromList cells}
  where
    cells = filter nextCell [(x, y) | x <- [0 .. mx], y <- [0 .. my]]
    nextCell c = case (gridAt g c, sumNeighbours g c) of
      (True, 2) -> True
      (_, 3) -> True
      _ -> False

nextfixed :: Grid -> Grid
nextfixed g@(Grid mx my gr) = g {gCells = S.union corners cells}
  where
    corners = S.fromList [(x, y) | x <- [0, mx], y <- [0, my]]
    g1 = g {gCells = S.union gr corners}
    Grid _ _ cells = next g1

-- >>> solve$ parse ".#.#.#\n...##.\n#....#\n..#...\n#.#..#\n####.."
-- (4,7)

-- solve :: Grid -> (Int, Int)
solve l = (S.size $ gCells $ turns !! 100, S.size $ gCells $ turns2 !! 100)
  where
    turns = iterate next l
    turns2 = iterate nextfixed l

main :: IO ()
main = readFile "input/day18.txt" >>= print . solve . parse
