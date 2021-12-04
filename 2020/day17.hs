#!/usr/bin/env stack
-- stack --resolver lts-10.2 script

{-# OPTIONS_GHC -Wall #-}

module Main where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

type Cell = (Int, Int, Int)

type Cell4 = (Int, Int, Int, Int)

type World = M.Map Cell Int

type World4 = M.Map Cell4 Int

parse :: String -> World
parse s = M.fromList [((x, y, 0), n) | (y, r) <- zip [0 ..] rows, (x, n) <- zip [0 ..] r]
  where
    parseChar c = if c == '#' then 1 else 0
    rows = fmap parseChar <$> lines s

parse4 :: String -> World4
parse4 s = M.fromList $ (\((x, y, z), v) -> ((x, y, z, 0), v)) <$> M.toList (parse s)

cell :: Ord k => M.Map k Int -> k -> Int
cell w c = fromMaybe 0 $ M.lookup c w

neighbours :: World -> Cell -> Int
neighbours w (x, y, z) = sum [cell w (a, b, c) | a <- r x, b <- r y, c <- r z, (a, b, c) /= (x, y, z)]
  where
    r n = [n -1 .. n + 1]

neighbours4 :: World4 -> Cell4 -> Int
neighbours4 w (x, y, z, t) =
  sum
    [ cell w nc
      | a <- r x,
        b <- r y,
        c <- r z,
        d <- r t,
        let nc = (a, b, c, d),
        nc /= (x, y, z, t)
    ]
  where
    r n = [n -1 .. n + 1]

nextCell :: World -> Cell -> Int
nextCell w c = case (cell w c, neighbours w c) of
  (1, 2) -> 1
  (_, 3) -> 1
  _ -> 0

nextCell4 :: World4 -> Cell4 -> Int
nextCell4 w c = case (cell w c, neighbours4 w c) of
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

boundaries4 :: World4 -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int))
boundaries4 w = (b xs, b ys, b zs, b ts)
  where
    cells = fst <$> M.toList w
    b a = (minimum a, maximum a)
    xs = [x | (x, _, _, _) <- cells]
    ys = [y | (_, y, _, _) <- cells]
    zs = [z | (_, _, z, _) <- cells]
    ts = [t | (_, _, _, t) <- cells]

next :: World -> World
next w = M.fromList cells
  where
    (xs, ys, zs) = boundaries w
    rng (x1, x2) = [x1 -1 .. x2 + 1]
    cells = [(k, v) | x <- rng xs, y <- rng ys, z <- rng zs, let k = (x, y, z), let v = nextCell w k]

next4 :: World4 -> World4
next4 w = M.fromList cells
  where
    (xs, ys, zs, ts) = boundaries4 w
    rng (x1, x2) = [x1 -1 .. x2 + 1]
    cells = [(k, v) | x <- rng xs, y <- rng ys, z <- rng zs, t <- rng ts, let k = (x, y, z, t), let v = nextCell4 w k]

-- >>> solve ".#.\n..#\n###"
-- (112,848)

solve :: String -> (Int, Int)
solve s = (first, second)
  where
    first = sum $ snd <$> M.toList (go input next 6)
    go w f n = foldr1 (\_ x -> f x) $ replicate (n + 1) w
    second = sum $ snd <$> M.toList (go (parse4 s) next4 6)
    input = parse s

main :: IO ()
main = readFile "input/day17.txt" >>= print . solve