#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Control.Arrow (Arrow (first, second))
import Data.Bifunctor (bimap)
import Data.List.Split (splitOn)
import Data.Set qualified as S

data Shape = Shape {sMaxx :: !Int, sMaxy :: !Int, sCont :: !(S.Set (Int, Int))} deriving (Eq)

type World = Shape

instance Show Shape where
  show (Shape mx my ps) = show (mx + 1) <> " x " <> show (my + 1) <> "\n" <> unlines (dline <$> [0 .. my])
    where
      dline y = [c | x <- [0 .. mx], let c = if S.member (x, y) ps then '#' else '.']

parse :: String -> String
parse = id

shapes :: [Shape]
shapes = flipy . createShape <$> [minus, plus, l, i, square]
  where
    createShape l = Shape (maximum $ fst <$> l) (maximum $ snd <$> l) $ S.fromList l
    minus = (,0) <$> [0 .. 3]
    plus = (0, 1) : (2, 1) : ((1,) <$> [0 .. 2])
    l = ((2,) <$> [0 .. 2]) <> ((,2) <$> [0 .. 2])
    i = (0,) <$> [0 .. 3]
    square = [(x, y) | x <- [0 .. 1], y <- [0 .. 1]]

flipy :: Shape -> Shape
flipy s@(Shape _ my se) = s {sCont = S.fromList $ second (my -) <$> S.toList se}

translateSh :: S.Set (Int, Int) -> (Int, Int) -> S.Set (Int, Int)
translateSh s (dx, dy) = S.fromList $ bimap (+ dx) (+ dy) <$> S.toList s

maxdepth = 50

clipWorld :: World -> World
clipWorld s@(Shape _ my cont) = s {sCont = S.filter ((> my - maxdepth) . snd) cont}

addShape :: World -> Shape -> String -> (String, World)
addShape world@(Shape maxwx maxwy wo) shape@(Shape maxsx maxsy sh) = go sx sy
  where
    sx = 2
    sy
      | S.null wo = 3 + maxsy
      | otherwise = 4 + maxwy

    move wd x y
      | x' == x = x
      | clashes x' y = x
      | otherwise = x'
      where
        x' = case wd of
          '>' | x < (maxwx - maxsx) -> x + 1
          '<' | x > 0 -> x - 1
          _ -> x

    mergeSh x y = world {sMaxy = max (y + maxsy) maxwy, sCont = S.union wo $ translateSh sh (x, y)}
    clashes x y = not $ S.null $ S.intersection (translateSh sh (x, y)) wo

    go x y dir
      | maxwy - y > maxdepth = undefined
      | y == 0 = (dir', clipWorld $ mergeSh x' y)
      | clashes x' y' = (dir', clipWorld $ mergeSh x' y)
      | otherwise = go x' y' dir'
      where
        d = head dir
        dir' = tail dir
        x' = move d x y
        y' = y - 1

tetris :: Int -> World -> [Shape] -> String -> (World, String, [Shape])
tetris n w sl mo = case (n, sl) of
  (0, _) -> (w, mo, sl)
  (_, []) -> (w, mo, sl)
  (_, s : ss) -> tetris (n - 1) w' ss mo'
    where
      (mo', w') = addShape w s mo

repeatsend :: Int -> [Int] -> Maybe ([Int], [Int])
repeatsend plen l = case splitOn patt main of
  [p, []] -> Just (p, patt)
  [p, ps] -> Just (take (length p - length ps) p, ps <> patt)
  _ -> Nothing
  where
    ofs = length l - plen
    patt = drop ofs l
    main = take ofs l
    a = splitOn patt main

-- a bit bruteforceish, found much faster solutions in AOC reddit - did get the job done in about 6 min
longTetrisLen :: Int -> World -> [Shape] -> String -> Int
longTetrisLen numshapes w shapes jets = 1 + sMaxy finalw
  where
    (finalw, _, _) = go2 w (cycle shapes) (cycle jets) []
    cyclelen = length shapes * length jets -- how often both shapes and jets go back to start position
    go2 world sh je cycles
      | sum cycles > numshapes = undefined
      | otherwise = case repeatsend 3 cycles' of
          Nothing -> go2 world' sh' je' cycles'
          Just (prefix, period) -> tetris extrashapes world'' sh' je'
            where
              remaining = numshapes - (cyclelen * length cycles')
              periodlen = cyclelen * length period
              periodheight = sum period
              periods = div remaining periodlen
              extrashapes = mod remaining periodlen
              deltay = periods * periodheight
              world'' = Shape wx' (wy' + deltay) $ translateSh scont' (0, deltay)
      where
        (world'@(Shape wx' wy' scont'), je', sh') = tetris cyclelen world sh je
        dh = sMaxy world' - sMaxy world
        cycles' = cycles <> [dh]

-- >>> solve $ parse ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
-- (3068,1514285714288)

solve :: [Char] -> (Int, Int)
solve l = (p1 2022, p2 1000000000000)
  where
    initial = Shape 6 0 S.empty
    finalp1 n = w where (w, _, _) = tetris n initial (cycle shapes) (cycle l)
    p1 n = 1 + sMaxy (finalp1 n)
    p2 n = longTetrisLen n initial shapes l

main :: IO ()
main = readFile "input/day17.txt" >>= print . solve . parse
