#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import Data.List (nub)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Set as S

data GameMap c = GameMap {gmW :: Int, gmH :: Int, gmMap :: M.Map (Int, Int) c} deriving (Eq)

type GmSizes = M.Map Int Int

instance Functor GameMap where
  fmap f (GameMap w h m) = GameMap w h $ M.map f m

instance Show (GameMap Char) where
  show gm@(GameMap w h m) = unlines [[mapTile '.' gm (x, y) | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]

data Direction = U | D | L | R deriving (Show, Eq, Ord, Enum)

mapTile :: c -> GameMap c -> (Int, Int) -> c
mapTile def gm p = fromMaybe def $ M.lookup p $ gmMap gm

inMap :: GameMap c -> (Int, Int) -> Bool
inMap (GameMap w h _) (x, y) = 0 <= x && x < w && 0 <= y && y < h

parse :: String -> GameMap Char
parse s = GameMap w h $ M.fromList [((x, y), c) | (y, line) <- zip [0 ..] l, (x, c) <- zip [0 ..] line, c /= '.']
  where
    l = lines s
    w = length $ head l
    h = length l

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = ((x,) <$> [y - 1, y + 1]) <> ((,y) <$> [x - 1, x + 1])

-- unlike nub doesn't preserve order
fastnub :: (Ord a) => [a] -> [a]
fastnub = S.toAscList . S.fromList

reachable :: GameMap Char -> Int -> [(Int, Int)] -> [(Int, Int)]
reachable gm n l
  | n == 0 = l
  | otherwise = reachable gm (n - 1) l'
  where
    blocks = M.keysSet $ M.filter (== '#') $ gmMap gm
    candidates = l >>= neighbors
    l' = fastnub $ filter available $ filter (inMap gm) candidates
    available p = S.notMember p blocks

exitDir :: GameMap a -> (Int, Int) -> Maybe Direction
exitDir (GameMap w h _) (x, y)
  | x < (-1) || y < (-1) || x > w || y > h = Nothing
  | x == -1 = Just L
  | y == -1 = Just U
  | x == w = Just R
  | y == h = Just D
  | otherwise = Nothing

solveMap :: GameMap Char -> [(Int, Int)] -> GmSizes
solveMap gamemap entrypoints = go 0 (M.singleton 0 $ length entrypoints) [] entrypoints
  where
    blocks = M.keysSet $ M.filter (== '#') $ gmMap gamemap
    available p = S.notMember p blocks
    go n counts last l = case (l, done) of
      ([], _) -> counts
      (_, True) -> counts
      _ -> go n' counts' l l'
      where
        exitP p = (,(p, n')) <$> exitDir gamemap p
        candidates = fastnub $ filter available $ l >>= neighbors
        l' = filter (inMap gamemap) candidates
        n' = n + 1
        counts' = M.insert n' (length l') counts
        done = l' == last

mapSize :: Int -> GmSizes -> Int
mapSize n m
  | n == 0 = undefined
  | n < M.size m = m M.! n
  | otherwise = m M.! (M.size m - 2 + mod (n - M.size m) 2)

-- grids form a diamond,with a quarter like:
--
-- I
-- Gh
-- Efh
-- Cdfh
-- Abdfh
--  ACEGI
--
-- grid is n by n, n is odd, perimeter is empty. sample grid is dense, so the following won't work, but does with the sparse input.
-- uppercase letters -> enter from the middle of a side, exit in 1 cycle from its ends, for main axis
-- lowercase letter -> enter form a corner - exit in 2 cycles from the end of its sides, for the rest
-- we start adding new grids after (n+1)/2 steps on the main axis, after n+1 on the others
-- 0   - only grid ' '
-- (w+1/2) - add A
-- w+1 - add b
-- (w+1/2) + w - add C
-- 2n+1 - add d
-- (w+1/2) + 2n - add E
-- 3n +1 - add f
-- (w+1/2) + 3n - add G
-- 4n+1 - add h

mains w n = div (n + div w 2) w

secs w n = l * (l + 1) `div` 2 where l = (n - 1) `div` w

-- >>> mains 131 <$> [0,65,66,131,131+66,131*2+66,131*3+66,131*4+66]
-- [0,0,1,1,2,3,4,5]

calcLength gm@(GameMap w h m) i = maintile
  where
    middle = div w 2
    (mains, mainMod) = divMod (i + middle) w
    (secondarylen, secMod) = divMod (i - 1) w
    maintile = solveMap gm [(middle, middle)] -- assume S in middle of input
    axistiles = solveMap gm <$> [[(0, middle)], [(middle, 0)], [(w - 1, middle)], [(middle, h - 1)]]
    sectiles = solveMap gm <$> [[(0, 0)], [(w - 1, 0)], [(0, h - 1)], [(w - 1, h - 1)]]
    totlen =
      sum
        [ mapSize i maintile, -- main tile
          sum (mapSize secMod <$> axistiles), -- last tiles on the axises
          sum (mapSize (secMod + w) <$> axistiles), -- next-to-last tiles on the axises
          div (mains + 1) 2 * sum (mapSize mainMod <$> sectiles) -- complete odd tiles on the axises
        ]

-- >>> c <$> [0 .. 7]
-- [(0,0,0),(1,4,0),(2,4,4),(3,8,4),(4,8,16),(5,12,16),(6,12,36),(7,16,36)]

-- >>> solve 6 $ parse "...........\n.....###.#.\n.###.##..#.\n..#.#...#..\n....#.#....\n.##..S####.\n.##..#...#.\n.......##..\n.##.#.####.\n.##..##.##.\n..........."
-- ((5,6),(5,(5,5)))
solve n gm = (p2)
  where
    seed = head $ M.keys $ M.filter (== 'S') $ gmMap gm
    p1 = length $ reachable gm n [seed]
    p2 = calcLength gm 26501365 -- part2 gm [seed]
    p3 = (div (gmW gm) 2, seed) -- showGm 131 [(65, 0)] -- [(65,130),(65,0),(130,65),(0,65)]
    showGm num l = gm {gmMap = M.union (gmMap gm) $ M.fromList $ (,'O') <$> filter (inMap gm) (reachable gm num l)}

main :: IO ()
main = readFile "input/day21.txt" >>= print . solve 64 . parse
