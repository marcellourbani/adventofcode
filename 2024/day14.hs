#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

{-# LANGUAGE PackageImports #-}

module Main where

import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import "containers" Data.Map.Strict qualified as M

data Robot = Robot {rP :: (Int, Int), rV :: (Int, Int)} deriving (Show)

type Input = [Robot]

parse :: String -> Input
parse s = pl <$> lines s
  where
    pc c = (a, b) where [a, b] = read <$> splitOn "," c
    pl l = let [p, v] = splitOn " v=" $ drop 2 l in Robot (pc p) (pc v)

move :: Int -> Int -> Robot -> Robot
move w h (Robot (px, py) v@(vx, vy)) = Robot (mod (px + vx) w, mod (py + vy) h) v

moven :: Int -> Int -> Int -> Robot -> Robot
moven w h n (Robot (px, py) v@(vx, vy)) = Robot (mod (px + n * vx) w, mod (py + n * vy) h) v

draw :: Int -> Int -> [Robot] -> GameMap Char
draw w h rs = GameMap w h $ M.fromList $ (,'*') . rP <$> rs

data GameMap c = GameMap {gmW :: Int, gmH :: Int, gmMap :: M.Map (Int, Int) c} deriving (Eq)

instance Functor GameMap where
  fmap f (GameMap w h m) = GameMap w h $ M.map f m

instance Show (GameMap Char) where
  show gm@(GameMap w h m) = unlines [[mapTile '.' gm (x, y) | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]

instance {-# OVERLAPPABLE #-} (Show c) => Show (GameMap c) where
  show gm@(GameMap w h m) = show $ gm {gmMap = head . show <$> m}

mapTile :: c -> GameMap c -> (Int, Int) -> c
mapTile def gm p = fromMaybe def $ M.lookup p $ gmMap gm

part1 :: Int -> Int -> Input -> Int
part1 w h l = product stats -- draw w h valids
  where
    mh = div h 2
    mw = div w 2
    valids = [r | r@(Robot (px, py) _) <- moven w h 100 <$> l, px /= mw, py /= mh]
    stats = M.unionsWith (+) [M.singleton (px > mw, py > mh) 1 | Robot (px, py) _ <- moven w h 100 <$> l, px /= mw, py /= mh]

-- found at 7709 - perhaps looking for this in a line would work: *******************************
part2 :: Int -> Int -> [Robot] -> IO String
part2 w h l = go 0
  where
    mv rs = move w h <$> rs
    go n = do
      let nn = 33 + 101 * n -- observing patterns I noticed robots get deser every 101
      print (n, nn)
      print $ draw w h (moven w h nn <$> l)
      print "\n\n"
      if n == 1000 then return "" else go (n + 1)

-- >>> part1 11 7 $ parse "p=0,4 v=3,-3\np=6,3 v=-1,-3\np=10,3 v=-1,2\np=2,0 v=2,-1\np=0,0 v=1,3\np=3,0 v=-2,-2\np=7,6 v=-1,-3\np=3,0 v=-1,-2\np=9,3 v=2,3\np=7,3 v=-1,2\np=2,4 v=2,-3\np=9,5 v=-3,-3"
-- 12

solve :: Int -> Int -> [Robot] -> IO ()
solve w h l = do
  print $ part1 w h l
  part2 w h l
  return ()

main :: IO ()
main = do
  f <- readFile "input/day14.txt"
  let i = parse f
  solve 101 103 i
  return ()
