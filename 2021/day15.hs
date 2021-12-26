#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (minimumBy)
import Data.Function (on)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import qualified Data.Vector as V

type Grid = V.Vector (V.Vector Int)

data Input = Input {maxx :: Int, maxy :: Int, grid :: Grid} deriving (Show)

gridAt :: Grid -> Int -> Int -> Int
gridAt g x y = g V.! y V.! x

parse :: String -> Input
parse s = Input {maxx = mx, maxy = my, grid = V.fromList $ V.fromList <$> depthlist}
  where
    depthlist = map (read . (: [])) <$> lines s
    my = length depthlist - 1
    mx = length (head depthlist) - 1

extend :: Input -> Input
extend (Input mx my gr) = Input mx' my' gr''
  where
    mx' = 5 * (mx + 1) -1
    my' = 5 * (my + 1) -1
    w = mx + 1
    h = my + 1
    trim x = ((x - 1) `mod` 9) + 1
    updLine vec i = trim . (+ i) <$> vec
    extendLine vec = vec V.++ V.concat (updLine vec <$> [1 .. 4])
    gr' = extendLine <$> gr
    updRows g i = updLine <$> g <*> pure i
    gr'' = gr' V.++ V.concat (updRows gr' <$> [1 .. 4])

dijkstraC :: Input -> M.Map (Int, Int) Int
dijkstraC (Input mx my gr) = go S.empty $ M.singleton (0, 0) 0
  where
    go visited acc
      | candidates == M.empty = acc
      | M.member (mx, my) acc = acc
      | otherwise = go visited' acc'
      where
        candidates = M.filterWithKey (\k _ -> S.notMember k visited) acc
        ((cx, cy), cv) = minimumBy (on compare snd) $ M.toList candidates
        visited' = S.insert (cx, cy) visited
        xs = [max 0 $ cx -1 .. min mx $ cx + 1]
        ys = [max 0 $cy -1 .. min my $cy + 1]
        toUpd = [v | x <- xs, y <- ys, let v = (x, y), M.notMember v acc, x == cx || y == cy]
        updates = M.fromList $ upd <$> toUpd
        acc' = M.union acc updates
        upd k = (k, cv + uncurry (gridAt gr) k)

-- >>> solve $parse "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581"
-- (40,315)

solve :: Input -> (Int, Int)
solve (Input mx my g) = (distances M.! (mx, my), distances2 M.! (mx', my'))
  where
    distances = dijkstraC (Input mx my g)
    (Input mx' my' g') = extend (Input mx my g)
    distances2 = dijkstraC (Input mx' my' g')

main :: IO ()
main = readFile "input/day15.txt" >>= print . solve . parse
