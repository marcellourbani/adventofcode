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

solveMap :: GameMap Char -> [(Int, Int)] -> (M.Map Int Int, M.Map Direction ((Int, Int), Int))
solveMap gamemap entrypoints = go 0 (M.singleton 0 $ length entrypoints) [] M.empty entrypoints
  where
    blocks = M.keysSet $ M.filter (== '#') $ gmMap gamemap
    available p = S.notMember p blocks
    nearexits = S.fromList $ mapMaybe (exitDir gamemap) $ entrypoints >>= neighbors
    relevantExits = S.difference (S.fromList [U .. R]) nearexits
    go n counts last exits l = case (l, done) of
      ([], _) -> (counts, exits)
      (_, True) -> (counts, exits'')
      _ -> go n' counts' l exits'' l'
      where
        exitP p = (,(p, n')) <$> exitDir gamemap p
        candidates = fastnub $ filter available $ l >>= neighbors
        exits' = M.union exits $ M.fromList $ mapMaybe exitP candidates
        exits'' = M.restrictKeys exits' relevantExits
        l' = filter (inMap gamemap) candidates
        n' = n + 1
        counts' = M.insert n' (length l') counts
        done = l' == last && M.size exits'' == S.size relevantExits

-- grids form a diamond,like:
--
--          G
--       fffEfff
--       fddCddf
--       fdbAbdf
--      GECA ACEG
--       fdbAbdf
--       fddCddf
--       fffEfff
--          G
-- grid is n by n, n is odd, perimeter is empty. sample grid is dense, so the following won't work, but does with the sparse input.
-- we add new grids every (n+1)/2 steps, call it cycle.
-- uppercase letters -> enter from the middle of a side, exit in 1 cycle from its ends
-- lowercase letter -> enter form a corner - exit in 2 cycles from the end of its sides
-- cycle 0 - only grid ' '
-- cycle 1 - add A
-- cycle 2 - add b
-- cycle 3 - add C
-- cycle 4 - add d
-- cycle 5 - add E
-- cycle 6 - add f
-- cycle 7 - add G

mainaxis n = 4 * div (n + 1) 2

secaxis n = 4 * (div n 2 ^ 2)

a = mainaxis <$> [0 .. 7]

b = secaxis <$> [0 .. 7]

c n = (n, mainaxis n, secaxis n)

calcLength gm@(GameMap w h m) i = undefined
  where
    seed = head $ M.keys $ M.filter (== 'S') $ gmMap gm
    middle = div w 2
    cyclelen = middle + 1
    maintile = solveMap gm [seed]
    axistiles = solveMap gm <$> [[(0, middle)], [(middle, 0)], [(w - 1, middle)], [(middle, h - 1)]]

-- >>> c <$> [0 .. 7]
-- [(0,0,0),(1,4,0),(2,4,4),(3,8,4),(4,8,16),(5,12,16),(6,12,36),(7,16,36)]

-- part2 :: GameMap Char -> Int -> Int
part2 gm@(GameMap w h m) i = entries2 $ entries [seed]
  where
    a = 1
    nxt ((x, y), d) = ((mod x w, mod y h), d)
    seed = head $ M.keys $ M.filter (== 'S') $ gmMap gm
    entries l = nxt . snd <$> M.toList (snd $ solveMap gm l)
    entries2 l = entries . (: []) . fst <$> l

-- >>> solve 6 $ parse "...........\n.....###.#.\n.###.##..#.\n..#.#...#..\n....#.#....\n.##..S####.\n.##..#...#.\n.......##..\n.##.#.####.\n.##..##.##.\n..........."
-- (1,5)
solve n gm = (p2, p3)
  where
    seed = head $ M.keys $ M.filter (== 'S') $ gmMap gm
    p1 = length $ reachable gm n [seed]
    p2 = 1 -- part2 gm [seed]
    p3 = (div (gmW gm) 2, seed) -- showGm 131 [(65, 0)] -- [(65,130),(65,0),(130,65),(0,65)]
    showGm num l = gm {gmMap = M.union (gmMap gm) $ M.fromList $ (,'O') <$> filter (inMap gm) (reachable gm num l)}

main :: IO ()
main = readFile "input/day21.txt" >>= print . solve 64 . parse

-- [((65,130),131),((130,130),66),((0,130),66)]
-- [((65,0),131),((130,0),66),((0,0),66)]
-- [((130,130),66),((130,0),66),((130,65),131)]
-- [((0,130),66),((0,0),66),((0,65),131)]

-- >>>  divMod (26501365) 66
-- (401535,55)
