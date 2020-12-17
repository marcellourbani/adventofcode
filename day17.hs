#!/usr/bin/env stack
-- stack --resolver lts-10.2 script

module Main where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

type Cell = (Int, Int, Int)

type World = M.Map Cell Int

parse :: String -> World
parse s = M.fromList [((x, y, 0), n) | (y, r) <- zip [0 ..] rows, (x, n) <- zip [0 ..] r]
  where
    parseChar c = if c == '#' then 1 else 0
    rows = fmap parseChar <$> lines s

cell :: World -> Cell -> Int
cell w c = fromMaybe 0 $ M.lookup c w

neighbours :: World -> Cell -> Int
neighbours w (x, y, z) = sum [cell w (a, b, c) | a <- r x, b <- r y, c <- r z, (a, b, c) /= (x, y, z)]
  where
    r n = [n -1 .. n + 1]

nextCell :: World -> Cell -> Int
nextCell w c = case (cell w c, neighbours w c) of
  (1, 2) -> 1
  (_, 3) -> 1
  _ -> 0

boundaries :: World -> ((Int, Int), (Int, Int), (Int, Int))
boundaries w = (b xs, b ys, b zs)
  where
    cells = fst <$> M.toList w
    b a = (minimum a, maximum a)
    xs = [x | (x, _, _) <- cells]
    ys = [y | (_, y, _) <- cells]
    zs = [z | (_, _, z) <- cells]

next :: World -> World
next w = M.fromList cells
  where
    (xs, ys, zs) = boundaries w
    rng (x1, x2) = [x1 -1 .. x2 + 1]
    cells = [(k, v) | x <- rng xs, y <- rng ys, z <- rng zs, let k = (x, y, z), let v = nextCell w k]

-- >>> solve ".#.\n..#\n###"
-- (112,3)

-- solve :: String -> (Int, Int)
solve s = (first, second)
  where
    first = sum $ snd <$> M.toList (go input 6)
    go w n
      | n == 0 = w
      | otherwise = go (next w) $ n - 1
    second = neighbours input (2, 1, 0)
    input = parse s

main :: IO ()
main = readFile "input/day17.txt" >>= print . solve