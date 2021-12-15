-- stack --resolver lts-18.18 script

module Main where

import qualified Data.Vector as V

type Grid = V.Vector (V.Vector Int)

data Input = Input {maxx :: Int, maxy :: Int, grid :: Grid} deriving (Show)

gridAt :: Grid -> Int -> Int -> Int
gridAt g x y = g V.! y V.! x

parse :: String -> Input
parse s = Input {maxx = mx, maxy = my, grid = V.fromList $ V.fromList <$> depthlist}
  where
    depthlist :: [[Int]]
    depthlist = map (read . (: [])) <$> lines s
    my = length depthlist - 1
    mx = length (head depthlist) - 1

minPath :: Input -> (Int, [(Int, Int)])
minPath (Input mx my gr) = go 0 0 (0, [])
  where
    go x y acc = case (x == mx, y == my) of
      (True, True) -> acc
      (True, _) -> go x (y + 1) acc'
      (_, True) -> go (x + 1) y acc'
      _ -> if fst cx < fst cy then cx else cy
        where
          cx = go (x + 1) y acc'
          cy = go x (y + 1) acc'
      where
        acc' = (fst acc + curcost, (x, y) : snd acc)
        curcost = gridAt gr x y

-- >>> minPath  $parse "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581"
-- (40,[(9,8),(8,8),(8,7),(8,6),(8,5),(7,5),(7,4),(7,3),(6,3),(6,2),(5,2),(4,2),(3,2),(2,2),(1,2),(0,2),(0,1),(0,0)])

solve :: Input -> (Int, Int)
solve i = (fst $ minPath i, 0)

main :: IO ()
main = readFile "input/day15.txt" >>= print . solve . parse
