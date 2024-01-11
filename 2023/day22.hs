#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import Data.Binary.Get (label)
import Data.List (nub, partition, sortOn)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing, mapMaybe)
import qualified Data.Set as S
import Data.Tuple (swap)

data Vector = Vector {vX :: Int, vY :: Int, vZ :: Int} deriving (Show, Eq, Ord)

data Brickg v = Brick {bId :: Int, bSt :: v, bEnd :: v} deriving (Show, Eq, Ord)

type Brick = Brickg Vector

instance Functor Brickg where
  fmap f (Brick i a b) = Brick i (f a) (f b)

parse :: String -> [Brick]
parse s = normalise . parseBrick <$> zip [1 ..] (lines s)
  where
    parseVect a = Vector x y z where [x, y, z] = read <$> splitOn "," a
    parseBrick (i, a) = Brick i b c where [b, c] = parseVect <$> splitOn "~" a
    normalise (Brick i a b) = Brick i (min a b) (max a b)

overlap :: Brick -> Brick -> Bool
overlap a b = overl vX && overl vY
  where
    bw a1 a2 b1 b2 = b1 <= a2 && a1 <= b2
    overl f = bw a1 a2 b1 b2 where (Brick _ a1 a2, Brick _ b1 b2) = (f <$> a, f <$> b)

maxZ :: Brick -> Int
maxZ = vZ . bEnd

minZ :: Brick -> Int
minZ = vZ . bSt

zDist :: Brick -> Brick -> Int
zDist a b = minZ a - maxZ b

isUnder :: Brick -> Brick -> Bool
isUnder a b = minZ a > maxZ b && overlap a b

unders :: [Brick] -> M.Map Int [Int]
unders l = M.unionsWith (<>) [M.singleton (bId b) [bId u] | b <- l, u <- filter (isUnder b) l]

dropz :: M.Map Int [Int] -> M.Map Int Brick -> M.Map Int Brick
dropz underM bricks = go bricks lowBs
  where
    lowBs = sortOn minZ $ snd <$> M.toList bricks
    under l b = (l M.!) <$> M.findWithDefault [] (bId b) underM
    dest l b = 1 + maximum (0 : (maxZ <$> under l b))
    toDest d b@(Brick i v1 v2) = Brick i (v1 {vZ = d}) (v2 {vZ = d + vZ v2 - vZ v1})
    lower l b = toDest (dest l b) b
    go lows rest = case rest of
      [] -> lows
      (x : xs) -> go (M.insert (bId x) (lower lows x) lows) xs

supportedBy :: M.Map Int [Int] -> M.Map Int Brick -> M.Map Int [Int]
supportedBy underM bricks = M.unionsWith (<>) l
  where
    supports a b = 1 == zDist (bricks M.! a) (bricks M.! b)
    l = [M.singleton bi [u] | (bi, us) <- M.toList underM, u <- us, supports bi u]

singleSupporters :: M.Map Int [Int] -> S.Set Int
singleSupporters m = S.fromList $ concatMap snd $ M.toList (M.filter ((== 1) . length) m)

allSupported :: M.Map Int [Int] -> M.Map Int [Int] -> Int -> [Int]
allSupported supporting supported i = nub $ go (S.singleton i) [i]
  where
    go preds parents = case nexts of
      [] -> []
      xs -> xs ++ go preds' xs
      where
        preds' = S.union preds $ S.fromList parents
        destroyed b = (all (`S.member` preds') <$> M.lookup b supported) == Just True
        aboves b = M.findWithDefault [] b supporting
        nexts = filter destroyed $ nub $ parents >>= aboves

-- >>> solve $ parse "1,0,1~1,2,1\n0,0,2~2,0,2\n0,2,3~2,2,3\n0,0,4~0,2,4\n2,0,5~2,2,5\n0,1,6~2,1,6\n1,1,8~1,1,9"
-- (5,7)

solve :: [Brick] -> (Int, Int)
solve l = (p1, p2)
  where
    bMap = M.fromList [(bId b, b) | b <- l]
    supported = supportedBy underM lowered
    singles = singleSupporters supported
    canDelete i = i `S.notMember` singles
    underM = unders l
    supportingM = M.unionsWith (<>) [M.singleton u [b] | (b, us) <- M.toList supported, u <- us]
    lowered = dropz underM bMap
    p1 = length $ filter canDelete $ M.keys lowered
    p2 = sum (length . allSupported supportingM supported . bId <$> lowered)

main :: IO ()
main = readFile "input/day22.txt" >>= print . solve . parse
