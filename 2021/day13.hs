#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (Foldable (foldl', foldr'))
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

data Fold = X Int | Y Int deriving (Show)

data Paper = Paper {maxx :: Int, maxy :: Int, points :: S.Set (Int, Int)}

data Input = Input {paper :: Paper, folds :: [Fold]} deriving (Show)

instance Show Paper where
  show (Paper maxx maxy m) = "maxx: " <> show maxx <> " maxy: " <> show maxy <> "\n" <> grid
    where
      grid = unlines $ drawLine <$> [0 .. maxy]
      drawLine y = toChar <$> linePoints
        where
          linePoints = zip [0 .. maxx] $ repeat y
          toChar p = if S.member p m then '#' else '.'

parse :: String -> Input
parse s = Input (Paper mx my $ S.fromList dots) foldops
  where
    [d, f] = splitOn "\n\n" s
    createDot l = case l of
      [x, y] -> (x, y)
      _ -> error "bad dot"
    dots = createDot . map read . splitOn "," <$> lines d
    createFold l = case l of
      ["x", n] -> X $ read n
      ["y", n] -> Y $ read n
      _ -> error "bad fold"
    foldops = createFold . splitOn "=" . (!! 2) . words <$> lines f
    mx = maximum $ fst <$> dots
    my = maximum $ snd <$> dots

foldPaper :: Paper -> Fold -> Paper
foldPaper (Paper mx my ps) f = case f of
  X n -> Paper n my newPoints
    where
      newPoints = S.fromList $ zip (foldDir n . fst <$> points) $snd <$> points
  Y n -> Paper mx n newPoints
    where
      newPoints = S.fromList $ zip (fst <$> points) $foldDir n . snd <$> points
  where
    foldDir line v = if v > line then 2 * line - v else v
    points = S.toList ps

-- >>> solve  $parse "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5"

-- solve :: Input -> (Int, Int)
solve (Input paper fs) = (S.size s, p)
  where
    p = foldl' foldPaper paper fs
    (Paper x y s) = foldPaper paper $head fs

main :: IO ()
main = readFile "input/day13.txt" >>= print . solve . parse
