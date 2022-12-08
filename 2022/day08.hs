#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import qualified Data.Map.Strict as M

type Input = M.Map (Int, Int) Int

parse :: String -> Input
parse s = M.fromList [((x, y), read [h]) | (y, l) <- zip [0 ..] $lines s, (x, h) <- zip [0 ..] l]

countTrees :: Input -> Int
countTrees tm = M.size visibles
  where
    (mx, my) = maximum $ M.keys tm
    visibles = M.fromList $ ([0 .. my] >>= line hkeys) <> ([0 .. mx] >>= line vkeys)
    be i l h = case l of
      [] -> []
      (x : xs)
        | x == 9 -> [True]
        | otherwise -> (i || x > h) : be False xs (max x h)
    hkeys y = zip [0 .. mx] (repeat y)
    vkeys x = zip (repeat x) [0 .. my]
    line kf c = filter snd $ lr <> rl
      where
        keys = kf c
        tl = M.findWithDefault 0 <$> keys <*> [tm]
        lr = zip keys $be True tl 0
        rl = zip (reverse keys) $be True (reverse tl) 0

scenicScore :: Input -> (Int, Int) -> Int
scenicScore tm (x, y)
  | hh == 0 || hh == 9 = 0
  | otherwise = product [lefts, rights, ups, downs]
  where
    (mx, my) = maximum $ M.keys tm
    height l = M.findWithDefault 0 l tm
    hh = height (x, y)
    lefts = visibles 1 $ zip [x -1, x -2 .. 0] (repeat y)
    rights = visibles 1 $ zip [x + 1 .. mx] (repeat y)
    ups = visibles 1 $ zip (repeat x) [y -1, y -2 .. 0]
    downs = visibles 1 $ zip (repeat x) [y + 1 .. my]
    visibles d keys = max 1 $ case keys of
      [] -> d - 1
      x : xs
        | ch == hh -> d
        | ch > hh -> if d < 3 then d else d -1
        | otherwise -> visibles (d + 1) xs
        where
          ch = height x

-- >>> solve $ parse "30373\n25512\n65332\n33549\n35390"
-- (21,16)

solve :: Input -> (Int, Int)
solve s = (p1, p2)
  where
    p1 = countTrees s
    p2 = maximum $ scenicScore s <$> M.keys s
    p2b = zip (M.keys s) $ scenicScore s <$> M.keys s
    ss = parse "30373\n25512\n65332\n33549\n35390"
    p2c = scenicScore ss (2, 3) --( 3,0)

main :: IO ()
main = readFile "input/day08.txt" >>= print . solve . parse
