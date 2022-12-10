#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Vector = (Int, Int, Int)

data Particle = Particle {pos :: Vector, vel :: Vector, acc :: Vector} deriving (Show, Eq, Ord)

parse :: String -> [Particle]
parse s = pl <$> lines s
  where
    sl l = splitOn ", " $filter (`notElem` "<>pva=") l
    pt t = (a, b, c) where [a, b, c] = read <$> splitOn "," t
    pl l = Particle a b c where [a, b, c] = pt <$> sl l

manhattan :: Vector -> Int
manhattan (x, y, z) = sum $ abs <$> [x, y, z]

isqrt :: Int -> Maybe Int
isqrt a
  | a == b * b = Just b
  | otherwise = Nothing
  where
    ai = fromIntegral a
    b = floor $ sqrt $ fromIntegral a

quadratic :: Int -> Int -> Int -> [Int]
quadratic a b c
  | a == 0 && b == 0 = []
  | a == 0 && mod c b == 0 = [div (- c) b]
  | a == 0 = []
  | otherwise = case isqrt $(b * b) - (4 * a * c) of
    Nothing -> []
    Just d -> [t | x <- [- b - d, - b + d], let t = div x di, mod x di == 0, t >= 0]
  where
    di = 2 * a

addVec :: Vector -> Vector -> Vector
addVec (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

subVec :: Vector -> Vector -> Vector
subVec (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

next :: Particle -> Particle
next (Particle p v a) = Particle p' v' a
  where
    v' = addVec v a
    p' = addVec p v'

intersection :: Particle -> Particle -> M.Map Int (S.Set Particle)
intersection pa1@(Particle p1 v1 a1) pa2@(Particle p2 v2 a2) = M.fromList $ zip times $ repeat $ S.fromList [pa1, pa2]
  where
    a'@(ax, ay, az) = subVec a2 a1
    v'@(vx, vy, vz) = subVec v2 v1
    p'@(px, py, pz) = subVec p2 p1
    times = S.toList $ go [(ax, vx, px), (ay, vy, py), (az, vz, pz)] Nothing
    inte (acc, v, p) = quadratic a b c
      where
        a = acc
        b = (2 * v) + acc
        c = 2 * p
    -- a = div acc 2
    -- b = acc + v - a
    go l s = case (l, s) of
      ([], Just ts) -> ts
      ([], Nothing) -> S.empty
      (_, Just ts) | S.null ts -> ts
      ((0, 0, 0) : xs, _) -> go xs s
      (x : xs, Nothing) -> go xs $ Just $ S.fromList $ inte x
      (x : xs, Just ts) -> go xs $ Just $ S.intersection ts $ S.fromList $ inte x

-- >>> solve $ parse "p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>\np=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>"
-- >>> solve $ parse "p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>\np=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>\np=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>\np=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>"
-- (0,fromList [(10,fromList [Particle {pos = (1779,-168,-944), vel = (-94,-42,0), acc = (14,-5,3)},Particle {pos = (2309,-1403,-859), vel = (-15,54,-69), acc = (-10,0,14)}])])
-- (0,fromList [(10,fromList [Particle {pos = (1779,-168,-944), vel = (-94,-42,0), acc = (14,-5,3)},Particle {pos = (2309,-1403,-859), vel = (-15,54,-69), acc = (-10,0,14)}])])

solve :: [Particle] -> (Int, Int)
solve l = (p1, p2)
  where
    accs = zip [0 ..] $manhattan . acc <$> l
    p1 = fst $ minimumBy (on compare snd) accs -- should check speeds on particles with same acceleration, but puzzle doesn't require it
    p2 = S.size $ S.difference (S.fromList l) $ S.unions $ M.elems $ go M.empty l
    go accu ps = case ps of
      [] -> accu
      x : xs -> go (M.unionsWith S.union $ accu : (intersection x <$> xs)) xs

main :: IO ()
main = readFile "input/day20.txt" >>= print . solve . parse
