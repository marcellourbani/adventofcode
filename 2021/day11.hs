#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (foldl', foldr')
import Data.List (find)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Input = Input {maxx :: Int, maxy :: Int, values :: M.Map (Int, Int) Int}

instance Show Input where
  show (Input maxx maxy m) = "maxx: " <> show maxx <> " maxy: " <> show maxy <> "\n" <> grid
    where
      grid = unlines $ drawLine <$> [0 .. maxy]
      cs = M.toList $ show <$> m
      drawLine y = mconcat $ snd <$> filter ((== y) . snd . fst) cs

parse :: String -> Input
parse s = Input maxx maxy $ M.fromList points
  where
    m = map (read . pure) <$> lines s
    points = [((x, y), v) | (y, l) <- zip [0 ..] m, (x, v) <- zip [0 ..] l]
    maxx = maximum $ fst . fst <$> points
    maxy = maximum $ snd . fst <$> points

nextGen :: Input -> (Input, Int)
nextGen (Input maxx maxy mm) = turn ((+ 1) <$> mm) S.empty
  where
    updateKeyList mm f kk = foldr' (M.updateWithKey (const $ Just . f)) mm kk
    turn m' flipped =
      if S.empty == newflips
        then (Input maxx maxy m', S.size flipped)
        else turn m''' f'''
      where
        (turnflips, m'') = flip m'
        newflips = S.difference turnflips flipped
        (f''', m''') = foldl' updneighbors (S.union flipped newflips, m'') newflips

    updneighbors (flipped, m') (x, y) = go flipped m' neighbors
      where
        go ff mm l = case l of
          [] -> (ff, mm)
          (k : ks) -> case (S.member k ff, M.lookup k mm) of
            (True, _) -> go ff mm ks
            (_, Nothing) -> go ff mm ks
            (_, Just v) -> go ff (M.insert k (v + 1) mm) ks
        neighbors = [(a, b) | a <- [x -1 .. x + 1], b <- [y -1 .. y + 1], (a, b) /= (x, y)]

    flip m' = (fk, updateKeyList m' (const 0) fk)
      where
        fk = S.fromList $ M.keys $ M.filter (>= 10) m'

-- >>> solve (parse "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526")
-- (1656,195)

-- solve :: Input -> (Int, Int)
solve (Input maxx maxy m) = (sum $take 101 $ snd <$> allflashes, maybe 0 fst firstcomplete)
  where
    numcells = (maxx + 1) * (maxy + 1)
    allflashes = iterate (\(i, _) -> nextGen i) (Input maxx maxy m, 0)
    firstcomplete = find ((== numcells) . snd) $zip [0 ..] $ snd <$> allflashes

main :: IO ()
main = readFile "input/day11.txt" >>= print . solve . parse
