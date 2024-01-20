#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

-- import Data.Char (toUpper)
-- import Data.Foldable (find)
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S

data MapTile = S | V | H | NE | NW | SE | SW deriving (Show, Eq)

type Point = (Int, Int)

type GameMap = M.Map Point MapTile

data Segment = Segment {segStart :: Point, segEnd :: Point} deriving (Show, Eq, Ord)

mkSegment :: Point -> Point -> Segment
mkSegment a b = Segment (min a b) (max a b)

parseDir :: Char -> Maybe MapTile
parseDir d = case d of
  '|' -> Just V
  '-' -> Just H
  'L' -> Just NE
  'J' -> Just NW
  '7' -> Just SW
  'F' -> Just SE
  'S' -> Just S
  _ -> Nothing

parse :: String -> GameMap
parse s = M.fromList $ zip [0 ..] (lines s) >>= parseLine
  where
    parseLine (y, l) = valids $ zip ((,y) <$> [0 ..]) $ parseDir <$> l
    valids l = case l of
      [] -> []
      (_, Nothing) : xs -> valids xs
      (a, Just t) : xs -> (a, t) : valids xs

manhattan :: Point -> Point -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

segLength :: Segment -> Int
segLength (Segment s e) = 1 + manhattan s e

validLocation :: Point -> Point -> MapTile -> Bool
validLocation (sx, sy) (ex, ey) d = case (d, ex - sx, ey - sy) of
  (H, 1, 0) -> True
  (H, -1, 0) -> True
  (V, 0, 1) -> True
  (V, 0, -1) -> True
  (NE, 0, 1) -> True
  (NW, 0, 1) -> True
  (SW, 0, -1) -> True
  (SE, 0, -1) -> True
  (SW, 1, 0) -> True
  (NW, 1, 0) -> True
  (NE, -1, 0) -> True
  (SE, -1, 0) -> True
  (S, _, _) -> True
  _ -> False

adjacents :: GameMap -> S.Set Point -> Point -> [(Point, MapTile)]
adjacents m blacklist p@(x, y) = mapMaybe vmap ls
  where
    vmap k = (k,) <$> M.lookup k m
    valid p1 = p1 /= p && S.notMember p1 blacklist
    ls = filter valid $ (,) <$> [x - 1 .. x + 1] <*> [y - 1 .. y + 1]

validAdjacents :: GameMap -> S.Set Point -> Point -> [(Point, MapTile)]
validAdjacents m blacklist p@(x, y) = filter valid base
  where
    base = adjacents m blacklist p
    cur = m M.! p
    valid (p1, t) = validLocation p p1 t && validLocation p1 p cur

calcDistances :: GameMap -> Point -> M.Map Point Int
calcDistances gamemap startpos = go initial [startpos] 1
  where
    initial = M.singleton startpos 0
    go curr ps dist
      | null newkeys = curr
      | otherwise = go nxt newkeys (dist + 1)
      where
        bl = M.keysSet curr
        newkeys = fst <$> (ps >>= validAdjacents gamemap bl)
        nxt = M.union curr $ M.fromList ((,dist) <$> newkeys)

isVertical :: Segment -> Bool
isVertical (Segment (x1, _) (x2, _)) = x1 == x2

joinSegments :: Segment -> Segment -> Maybe Segment
joinSegments se1@(Segment s1 e1) se2@(Segment s2 e2)
  | s2 == e1 && compatible = Just $ Segment s1 e2
  | s1 == e2 && compatible = Just $ Segment s2 e1
  | otherwise = Nothing
  where
    compatible = isVertical se1 == isVertical se2 || s1 == e1 || s2 == e2

calcPerimeter :: GameMap -> M.Map Point Int -> [Segment]
-- calcPerimeter gamemap distances = joinAll $ go 1 $ head $ atDistance M.! 0
calcPerimeter gamemap distances = go Nothing 1 $ head $ atDistance M.! 0
  where
    atDistance = M.unionsWith (++) [M.singleton d [p] | (p, d) <- M.toList distances]
    valid p1 p2 = validLocation p1 p2 (gamemap M.! p2) && validLocation p2 p1 (gamemap M.! p1)
    candidates p d = filter (valid p) $ M.findWithDefault [] d atDistance

    go last d p = if null cands then catMaybes [last] else cands >>= follow
      where
        cands = candidates p d
        segs = mkSegment p <$> cands
        nexts = if null cands then [] else cands >>= follow
        follow c = case (last, mkSegment p c) of
          (Nothing, s1) -> go (Just s1) (d + 1) c
          (Just s1, s2) -> case joinSegments s1 s2 of
            Nothing -> s1 : go (Just s2) (d + 1) c
            Just s -> go (Just s) (d + 1) c

segPoints :: Segment -> [Point]
segPoints (Segment (x1, y1) (x2, y2)) = (,) <$> [x1 .. x2] <*> [y1 .. y2]

enclosed :: [Segment] -> S.Set Point
enclosed segs = S.fromList $ [minimum allys .. maximum allys] >>= go
  where
    allys = (snd . segStart <$> segs) <> (snd . segEnd <$> segs)
    vertStarts = S.fromList $ segStart <$> filter isVertical segs
    isCrossing se@(Segment s e) = isVertical se || odd (length $ filter (`S.member` vertStarts) [s, e])
    shorten vs = case vs of
      [] -> []
      s@(Segment (x1, _) (x2, _)) : ss | x1 /= x2 -> s : shorten ss
      s@(Segment (_, y1) (_, y2)) : ss | y2 - y1 < 2 -> shorten ss
      s@(Segment (x1, y1) (_, y2)) : ss -> Segment (x1, y1 + 1) (x1, y2 - 1) : shorten ss
    unconnected = shorten segs
    ony y (Segment (_, y1) (_, y2)) = y >= y1 && y <= y2
    ysegs y = sort $ filter (ony y) unconnected
    xlen (Segment (x1, _) (x2, _)) = 1 + x2 - x1
    whitelisted = flip S.notMember (S.fromList $ segs >>= segPoints)
    betweenS y (Segment _ (x1, _)) (Segment (x2, _) _) = filter whitelisted $ (,y) <$> [x1 .. x2]
    enclosedInLine y ss = case ss of
      [] -> []
      [s] -> []
      s1 : s2 : ss'
        | isCrossing s1 && isCrossing s2 -> betweenS y s1 s2 ++ enclosedInLine y ss'
        | isCrossing s1 -> enclosedInLine y (s1 : ss')
        | otherwise -> enclosedInLine y (s2 : ss')
    go y = enclosedInLine y $ ysegs y

-- >>> solve $ parse "-L|F7\n7S-7|\nL|7||\n-L-J|\nL|-JF"
-- >>> solve $ parse "..F7.\n.FJ|.\nSJ.L7\n|F--J\nLJ..."
-- >>> solve $ parse ".F----7F7F7F7F-7....\n.|F--7||||||||FJ....\n.||.FJ||||||||L7....\nFJL7L7LJLJ||LJ.L-7..\nL--J.L7...LJS7F-7L7.\n....F-J..F7FJ|L7L7L7\n....L7.F7||L7|.L7L7|\n.....|FJLJ|FJ|F7|.LJ\n....FJL-7.||.||||...\n....L---J.LJ.LJLJ..."
-- >>> solve $ parse "FF7FSF7F7F7F7F7F---7\nL|LJ||||||||||||F--J\nFL-7LJLJ||||||LJL-77\nF--JF--7||LJLJIF7FJ-\nL---JF-JLJIIIIFJLJJ7\n|F|F-JF---7IIIL7L|7|\n|FFJF7L7F-JF7IIL---7\n7-L-JL7||F7|L7F-7F7|\nL.L7LFJ|||||FJL7||LJ\nL7JLJL-JLJLJL--JLJ.L"
-- (4,1)
-- (8,1)
-- (70,8)
-- (80,10)

solve :: GameMap -> (Int, Int)
solve gamemap = (p1, p2)
  where
    distances = calcDistances gamemap startP
    peri = calcPerimeter gamemap distances
    p1 = maximum distances
    p2 = length $ enclosed peri
    startP = fst . M.elemAt 0 $ M.filter (== S) gamemap

main :: IO ()
main = readFile "input/day10.txt" >>= print . solve . parse
