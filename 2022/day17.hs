#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Control.Arrow (Arrow (first, second))
import Data.Bifunctor (bimap)
import Data.List.Split (splitOn)
import qualified Data.Set as S

data Shape = Shape {sMaxx :: Int, sMaxy :: Int, sCont :: S.Set (Int, Int)} deriving (Eq)

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
    minus = zip [0 .. 3] (repeat 0)
    plus = (0, 1) : (2, 1) : zip (repeat 1) [0 .. 2]
    l = zip (repeat 2) [0 .. 2] <> zip [0 .. 2] (repeat 2)
    i = zip (repeat 0) [0 .. 3]
    square = [(x, y) | x <- [0 .. 1], y <- [0 .. 1]]

flipy :: Shape -> Shape
flipy s@(Shape _ my se) = s {sCont = S.fromList $ second (my -) <$> S.toList se}

translateSh :: S.Set (Int, Int) -> (Int, Int) -> S.Set (Int, Int)
translateSh s (dx, dy) = S.fromList $ bimap (+ dx) (+ dy) <$> S.toList s

addShape :: Shape -> Shape -> String -> (String, Shape)
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
          '<' | x > 0 -> x -1
          _ -> x

    mergeSh x y = world {sMaxy = max (y + maxsy) maxwy, sCont = S.union wo $ translateSh sh (x, y)}
    clashes x y = not $ S.null $ S.intersection (translateSh sh (x, y)) wo

    go x y dir
      | y == 0 = (dir', mergeSh x' y)
      | clashes x' y' = (dir', mergeSh x' y)
      | otherwise = go x' y' dir'
      where
        d = head dir
        dir' = tail dir
        x' = move d x y
        y' = y - 1

-- >>> solve $ parse ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
-- 3068

solve :: [Char] -> Int
solve l = p1 2022
  where
    initial = Shape 6 0 S.empty
    p1a n = bimap flipy (take 20) $ go n initial (cycle shapes) $ cycle l
    p1 n = 1 + sMaxy (fst $ go n initial (cycle shapes) (cycle l))
    go n w sl mo = case (n, sl) of
      (0, _) -> (w, mo)
      (_, []) -> (w, mo)
      (_, s : ss) -> go (n -1) w' ss mo'
        where
          (mo', w') = addShape w s mo

main :: IO ()
main = readFile "input/day17.txt" >>= print . solve . parse
