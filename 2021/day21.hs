#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (Foldable (foldr'), foldl')
import qualified Data.Map as M

data Board = Board {p1 :: Int, p2 :: Int, dice :: Int} deriving (Show, Eq)

type Input = Board

parse :: String -> Input
parse s = Board a b 0
  where
    [a, b] = read . (: "") . last <$> lines s

turn :: Board -> Bool -> Board
turn (Board pl1 pl2 d) p1Turn = Board pl1' pl2' d'
  where
    throw (cd, s) = (mod (cd + 1) 100, mod (s + cd) 10 + 1)
    is = if p1Turn then pl1 else pl2
    (d', score) = iterate throw (d, is) !! 3
    (pl1', pl2') = if p1Turn then (score, pl2) else (pl1, score)

game :: Board -> (Board, Int, Int)
game b = go b True 0 0 0
  where
    go (Board pl1 pl2 d) p1t sc1 sc2 rolls =
      case (sc1 >= 1000, sc2 >= 1000) of
        (True, _) -> (b, sc2, rolls)
        (_, True) -> (b, sc1, rolls)
        _ -> go b' (not p1t) sc1' sc2' (rolls + 3)
      where
        b' = turn (Board pl1 pl2 d) p1t
        sc1' = if p1t then sc1 + p1 b' else sc1
        sc2' = if p1t then sc2 else sc2 + p2 b'

addMaps :: [M.Map Int Int] -> M.Map Int Int
addMaps = foldl' (M.unionWith (+)) M.empty

addVals :: [(Int, Int)] -> M.Map Int Int
addVals l = addMaps $ uncurry M.singleton <$> l

md :: Int -> Int -> Int
md a b = 1 + mod (a -1) b

game2 :: Board -> (Int, Int)
game2 (Board pl1 pl2 _) = go (M.singleton (pl1, 0) 1) (M.singleton (pl2, 0) 1) True 0 0
  where
    go m1 m2 turn wins1 wins2 = case (m1 == M.empty || m2 == M.empty, turn) of
      (True, _) -> (wins1, wins2)
      (_, True) -> go m1' m2 False (wins1 + newwins) wins2
        where
          (m1', newwins) = turn2 m1 m2
      (_, False) -> go m1 m2' True wins1 (wins2 + newwins)
        where
          (m2', newwins) = turn2 m2 m1

    turn2 mcur moth = (mcur'', wins)
      where
        mcur' = score2 mcur
        (winm, mcur'') = M.partitionWithKey (const . (>= 21) . snd) mcur'
        wins = foldl' (+) 0 winm * foldl' (+) 0 moth

    score2 m = foldl' (M.unionWith (+)) M.empty factors
      where
        mul ((v1, s), f1) (v2, f2) = M.singleton (nv, s + nv) (f1 * f2) where nv = md (v1 + v2) 10
        factors = mul <$> M.toList m <*> rollFrequencies
    rollFrequencies = M.toList $ addVals scores
      where
        roll = [1 .. 3]
        scores = zip [a + b + c | a <- roll, b <- roll, c <- roll] $ repeat 1

-- >>> solve  $parse "Player 1 starting position: 4\nPlayer 2 starting position: 8"
-- (739785,444356092776315)

solve :: Board -> (Int, Int)
solve i = (other * rolls, max wins1 wins2)
  where
    (_, other, rolls) = game i
    (wins1, wins2) = game2 i

main :: IO ()
main = readFile "input/day21.txt" >>= print . solve . parse
