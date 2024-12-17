#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

{-# LANGUAGE PackageImports #-}

module Main where

import Control.Lens ((^.))
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Linear.V2
import "containers" Data.Map.Strict qualified as M
import "containers" Data.Set qualified as S

data GameMap c = GameMap {gmW :: Int, gmH :: Int, gmMap :: M.Map (V2 Int) c} deriving (Eq)

data Input = Input {iGM :: GameMap Char, iRob :: V2 Int, iDirs :: [Char]} deriving (Show)

instance Functor GameMap where
  fmap f (GameMap w h m) = GameMap w h $ M.map f m

instance Show (GameMap Char) where
  show gm@(GameMap w h m) = unlines [[mapTile '.' gm (V2 x y) | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]

instance {-# OVERLAPPABLE #-} (Show c) => Show (GameMap c) where
  show gm@(GameMap w h m) = show $ gm {gmMap = head . show <$> m}

mapTile :: c -> GameMap c -> V2 Int -> c
mapTile def gm p = fromMaybe def $ M.lookup p $ gmMap gm

inMap :: GameMap c -> V2 Int -> Bool
inMap (GameMap w h _) (V2 x y) = 0 <= x && x < w && 0 <= y && y < h

parseM :: String -> GameMap Char
parseM s = GameMap w h $ M.fromList [(V2 x y, c) | (y, line) <- zip [0 ..] l, (x, c) <- zip [0 ..] line, c /= '.']
  where
    l = lines s
    w = length $ head l
    h = length l

dirVec :: (Num a) => Char -> V2 a
dirVec d = case d of
  '>' -> V2 1 0
  '<' -> V2 (-1) 0
  '^' -> V2 0 (-1)
  'v' -> V2 0 1

parse :: String -> Input
parse a = Input (ma {gmMap = cm}) rp moves
  where
    [m, r] = splitOn "\n\n" a
    ma = parseM m
    (rm, cm) = M.partition (== '@') $ gmMap ma
    moves = filter (/= '\n') r
    rp = head $ M.keys rm

pushBox :: (GameMap Char, V2 Int) -> V2 Int -> (GameMap Char, V2 Int)
pushBox (gm, loc) dir = case moves of
  [] -> (gm, loc)
  l : ls -> (gm {gmMap = domoves (l : ls)}, l)
  where
    moves = (+ dir) <$> go loc []
    domoves (l : ls) = M.union (M.delete l (gmMap gm)) (M.fromList $ (,'O') <$> ls)
    go l acc
      | not $ inMap gm l = acc
      | otherwise = case mapTile '.' gm l' of
          '#' -> []
          '.' -> reverse $ l : acc
          _ -> go l' (l : acc)
      where
        l' = l + dir

pushBox2 :: (GameMap Char, V2 Int) -> V2 Int -> (GameMap Char, V2 Int)
pushBox2 g@(gm, loc) dir = res
  where
    mt = mapTile '.' gm
    box p = case mt p of
      ']' -> [p - V2 1 0, p]
      '[' -> [p, p + V2 1 0]
      _ -> [p]
    nexts ps = filter ((/= '.') . mt) $ S.toList $ S.fromList (ps >>= box . (+ dir))
    res = case S.toList $ go (S.singleton loc) (S.singleton loc) of
      [] -> (gm, loc)
      ls -> (gm {gmMap = m}, loc + dir)
        where
          news = M.fromList $ zip ((+ dir) <$> ls) (mt <$> ls)
          m = M.union news $ M.withoutKeys (gmMap gm) (S.fromList ls)
    go ps acc
      | S.member '#' cur = S.empty
      | S.null cur = acc
      | otherwise = go ps' acc'
      where
        ps' = S.difference (S.fromList $ nexts $ S.toList ps) acc
        cur = S.fromList $ mt <$> S.toList ps'
        acc' = S.union acc ps'

setRob :: (GameMap Char, V2 Int) -> GameMap Char
setRob (gm, r) = gm {gmMap = M.insert r '@' $ gmMap gm}

gps :: V2 Int -> Int
gps (V2 x y) = x + 100 * y

resize :: Input -> Input
resize (Input gm rob i) = Input gm' rob' i
  where
    r = V2 2 1
    dt c = case c of
      '#' -> "##"
      'O' -> "[]"
      _ -> ""
    double (v, c) = zip [v * r, v * r + V2 1 0] $ dt c
    rob' = rob * r
    gm' = gm {gmMap = M.fromList $ M.toList (gmMap gm) >>= double, gmW = 2 * gmW gm}

part1 :: Input -> Int
part1 (Input gm r directions) = score
  where
    nm = setRob $ go (gm, r) directions
    score = sum $ gps <$> M.keys (M.filter (== 'O') $ gmMap nm)
    go a dirs = case dirs of
      d : ds -> go (pushBox a $ dirVec d) ds
      _ -> a

part2 :: Input -> Int
part2 l = score
  where
    (Input gm r directions) = resize l
    nm = setRob $ go (gm, r) directions
    score = sum $ gps <$> M.keys (M.filter (== '[') $ gmMap nm)
    go a dirs = case dirs of
      d : ds -> go (pushBox2 a $ dirVec d) ds
      _ -> a

-- >>> solve $ parse "##########\n#..O..O.O#\n#......O.#\n#.OO..O.O#\n#..O@..O.#\n#O#..O...#\n#O..O..O.#\n#.OO.O.OO#\n#....O...#\n##########\n\n<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^\nvvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v\n><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<\n<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^\n^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><\n^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^\n>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^\n<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>\n^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>\nv^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"
-- >>> solve $ parse "########\n#..O.O.#\n##@.O..#\n#...O..#\n#.#.O..#\n#...O..#\n#......#\n########\n\n<^^>>>vv<v>>v<<"
-- (10092,9021)
-- (2028,1751)

solve :: Input -> (Int, Int)
solve l = (p1, p2)
  where
    p1 = part1 l
    p2 = part2 l

main :: IO ()
main = readFile "input/day15.txt" >>= print . solve . parse
