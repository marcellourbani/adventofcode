#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (Foldable (foldl'), find)
import qualified Data.Set as S

data Turn = L | R deriving (Show, Eq, Read)

data Dir = N | E | S | W deriving (Show, Eq, Enum, Ord)

data Bunny = Bunny Dir Int Int deriving (Show, Eq, Ord)

parse :: String -> [(Turn, Int)]
parse s = pw <$> words (filter (/= ',') s)
  where
    pw (x : xs) = (read [x], read xs)
    pw _ = error "invalid input"

moveBunny :: Bunny -> (Turn, Int) -> Bunny
moveBunny (Bunny d i j) (t, n) = Bunny d' i' j'
  where
    dn = fromEnum d
    d' = case t of
      L -> toEnum $ (dn -1) `mod` 4
      R -> toEnum $ (dn + 1) `mod` 4
    (i', j') = case d' of
      N -> (i, j + n)
      E -> (i + n, j)
      S -> (i, j - n)
      W -> (i - n, j)

-- >>> solve $ parse "R8, R4, R4, R8"
-- (8,4)

solve :: [(Turn, Int)] -> (Int, Int)
solve l = (lon + lat, go bunny (S.singleton (0, 0)) l)
  where
    bunny = Bunny N 0 0
    (Bunny _ lon lat) = foldl' moveBunny bunny l
    segment (Bunny _ x1 y1) (Bunny _ x2 y2) = [(x, y) | x <- xs, y <- ys, (x, y) /= (x1, y1)]
      where
        xs = if x1 <= x2 then [x1 .. x2] else [x1, x1 -1 .. x2]
        ys = if y1 <= y2 then [y1 .. y2] else [y1, y1 -1 .. y2]
    go b visited l = case l of
      [] -> 0
      x : xs -> case collision of
        Just (x, y) -> x + y
        _ -> go bunny' visited' xs
        where
          bunny'@(Bunny _ a c) = moveBunny b x
          visited' = S.union (S.fromList newCoords) visited
          newCoords = segment b bunny'
          collision = find (`S.member` visited) newCoords

main :: IO ()
main = readFile "input/day01.txt" >>= print . solve . parse
