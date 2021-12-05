#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.Bits ((.&.))
import Data.Bool (bool)
import Data.List (foldl1', tails, transpose)
import Data.List.Compat (findIndices)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S

data Tile = Tile {tid :: Int, ttab :: [[Bool]]} deriving (Eq, Ord)

instance Show Tile where
  show (Tile i t) = "Tile " ++ show i ++ ":\n" ++ unlines (map (bool '.' '#') <$> t)

data Orientation = Orientation {flipped :: Bool, rotaton :: Int} deriving (Show, Eq, Ord)

boolsToInt :: [Bool] -> Int
boolsToInt l = sum [2 ^ e | (d, e) <- zip l [0 :: Int ..], d]

orientations :: [Orientation]
orientations = [Orientation f n | f <- [False, True], n <- [0 .. 3]]

orientTile :: Tile -> Orientation -> Tile
orientTile (Tile ti t) (Orientation fl rot) = Tile ti $ foldr ($) t' (replicate (mod rot 4) $transpose . reverse)
  where
    t'
      | fl = reverse t
      | otherwise = t

parse :: String -> [Tile]
parse s = rawTiles
  where
    rawTiles = pt . lines <$> splitOn "\n\n" s
    pt l = Tile (tilenum $ head l) $ map (== '#') <$> tail l
    tilenum x = read . init $ (!! 1) . words $ x

topNum :: Tile -> Int
topNum = boolsToInt . head . ttab

-- >>> solve "Tile 2311:\n..##.#..#.\n##..#.....\n#...##..#.\n####.#...#\n##.##.###.\n##...#.###\n.#.#.#..##\n..#....#..\n###...#.#.\n..###..###\n\nTile 1951:\n#.##...##.\n#.####...#\n.....#..##\n#...######\n.##.#....#\n.###.#####\n###.##.##.\n.###....#.\n..#.#..#.#\n#...##.#..\n\nTile 1171:\n####...##.\n#..##.#..#\n##.#..#.#.\n.###.####.\n..###.####\n.##....##.\n.#...####.\n#.##.####.\n####..#...\n.....##...\n\nTile 1427:\n###.##.#..\n.#..#.##..\n.#.##.#..#\n#.#.#.##.#\n....#...##\n...##..##.\n...#.#####\n.#.####.#.\n..#..###.#\n..##.#..#.\n\nTile 1489:\n##.#.#....\n..##...#..\n.##..##...\n..#...#...\n#####...#.\n#..#.#.#.#\n...#.#.#..\n##.#...##.\n..##.##.##\n###.##.#..\n\nTile 2473:\n#....####.\n#..#.##...\n#.##..#...\n######.#.#\n.#...#.#.#\n.#########\n.###.#..#.\n########.#\n##...##.#.\n..###.#.#.\n\nTile 2971:\n..#.#....#\n#...###...\n#.#.###...\n##.##..#..\n.#####..##\n.#..####.#\n#..#.#..#.\n..####.###\n..#.#.###.\n...#.#.#.#\n\nTile 2729:\n...#.#.#.#\n####.#....\n..#.#.....\n....#..#.#\n.##..##.#.\n.#.####...\n####.#.#..\n##.####...\n##..#.##..\n#.##...##.\n\nTile 3079:\n#.#.#####.\n.#..######\n..#.......\n######....\n####.#..#.\n.#...#.##.\n#.#####.##\n..#.###...\n..#.......\n..#.###..."
-- (20899048083289,273)

solve :: String -> (Int, Int)
solve s = (first, second)
  where
    first = product $ fst <$> corners
    second = countTrues (ttab picture) - (numMonsters * countTrues monster)
    tiles = parse s
    possibleEdges (Tile ti t) = (ti, S.fromList $ topNum . orientTile (Tile ti t) <$> orientations)
    tileEdges = possibleEdges <$> tiles
    uniqueEdges = [(t1, e) | (t1, e1) <- tileEdges, let e = S.difference e1 $ S.unions $ snd <$> filter ((/= t1) . fst) tileEdges]
    edgeMap = M.fromListWith (++) [(e, [t]) | (t, es) <- tileEdges, e <- S.toList es]
    tileMap = M.fromList [(tid t, t) | t <- tiles]
    corners = filter ((== 4) . S.size . snd) uniqueEdges
    rotate tr r = orientTile tr $ Orientation False r
    flipTile tr = orientTile tr $ Orientation True 0
    firstCorner = topRight
      where
        (ti, ue) = head corners
        tt = flipTile $ tileMap M.! ti
        ors = [i | i <- [0 .. 3], let a = topNum $ rotate tt i, S.member a ue]
        topRight = orientTile tt $ Orientation False $ head [i | i <- ors, mod (i + 1) 4 `elem` ors]
    toright t = rt
      where
        shared = topNum $ rotate t 3
        nti = [ti | ti <- edgeMap M.! shared, ti /= tid t]
        nt = [rotate (flipTile t2) 1 | ti <- nti, let t1 = tileMap M.! ti, o <- orientations, let t2 = orientTile t1 o, topNum t2 == shared]
        rt
          | null nt = Nothing
          | otherwise = Just $ head nt

    line t = case toright t of
      Nothing -> [t]
      Just t1 -> t : line t1
    tobelow t = rt
      where
        shared = topNum $ rotate t 2
        nti = [ti | ti <- edgeMap M.! shared, ti /= tid t]
        nt = [rotate (flipTile t2) 2 | ti <- nti, let t1 = tileMap M.! ti, o <- orientations, let t2 = orientTile t1 o, topNum t2 == shared]
        rt
          | null nt = Nothing
          | otherwise = Just $ head nt
    column t = case tobelow t of
      Nothing -> [t]
      Just t1 -> t : column t1
    grid = map clearTile <$> map line (column firstCorner)
      where
        mid = tail . init
        clearTile (Tile i t) = Tile i (mid $ mid <$> t)
    joingrid = foldl1' (++) . map (foldl1' (zipWith (++)) . map ttab)
    picture = Tile 0 $ joingrid grid
    monster = map (== '#') <$> lines "                  # \n#    ##    ##    ###\n #  #  #  #  #  #   "
    imonster = boolsToInt <$> monster
    mlen = length $head monster
    matchline ml l = mlen == length chu && hasml
      where
        chu = take mlen l
        hasml = ml == boolsToInt chu .&. ml
    matinline ml l = findIndices (matchline ml) $ tails l
    countMonsters l = case l of
      (l1 : l2 : l3 : _) -> nm + countMonsters (tail l)
        where
          m1 = S.fromList $ matinline (head imonster) l1
          m2 = S.fromList $ matinline (imonster !! 1) l2
          m3 = S.fromList $ matinline (imonster !! 2) l3
          nm = S.size $ S.intersection m1 $ S.intersection m2 m3
      _ -> 0
    numMonsters = maximum $ countMonsters . ttab . orientTile picture <$> orientations
    countTrues l = sum $ map (sum . map (\x -> if x then 1 else 0)) l

main :: IO ()
main = readFile "input/day20.txt" >>= print . solve