#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE TupleSections #-}

module Main where

import Data.ByteString.Char8 (pack)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.PQueue.Prio.Min as P
import qualified Data.Set as S
import Distribution.Utils.MD5 (md5, showMD5)

data Room = Room {rpath :: String, rx :: Int, ry :: Int} deriving (Show, Eq, Ord)

parse :: String -> String
parse = id

md5s :: String -> String
md5s = showMD5 . md5 . pack

nextRooms :: Room -> [Room]
nextRooms (Room path x y) = [Room (path <> [p]) a b | (p, a, b) <- steps]
  where
    dirs = fst <$> filter ((`elem` "bcdef") . snd) (zip "UDLR" $ take 4 $ md5s path)
    allsteps = [('U', x, y -1), ('D', x, y + 1), ('L', x - 1, y), ('R', x + 1, y)]
    steps = [(p, a, b) | (p, a, b) <- allsteps, a >= 0, a <= 3, b >= 0, b <= 3, p `elem` dirs]

aStar :: Ord s => P.MinPQueue Int (s, Int) -> S.Set s -> (s -> Bool) -> (s -> Int -> [(Int, (s, Int))]) -> Maybe (s, Int)
aStar queue visited goal nextStates
  | P.null queue = Nothing
  | goal curst = Just (curst, curcost)
  | S.member curst visited = aStar queue' visited goal nextStates
  | otherwise = aStar queue'' visited' goal nextStates
  where
    (_, (curst, curcost)) = P.findMin queue
    queue' = P.deleteMin queue
    visited' = S.insert curst visited
    nexts l = case l of
      [] -> []
      (e, (s, c)) : xs
        | S.member s visited' -> nexts xs
        | otherwise -> (e, (s, c)) : nexts xs
    queue'' = P.union queue' $ P.fromList $ nexts $ nextStates curst curcost

roomCost :: Room -> Int
roomCost (Room _ x y) = 3 - x + 3 - y

nextCostedRoom :: Room -> Int -> [(Int, (Room, Int))]
nextCostedRoom r ic = (ic + roomCost r,) . (,ic + 1) <$> nextRooms r

-- >>> solve $ parse "ulqzkmiv"
-- ("DRURDRUDDLLDLUURRDULRLDUUDDDRR",Just 830)

solve :: String -> (String, Maybe Int)
solve l = (p1 start, p2 start)
  where
    p1 s = maybe "" (drop (length $ rpath s) . rpath . fst) $ solve s
    start = Room l 0 0
    solve s = aStar (P.singleton (roomCost s) (s, 0)) S.empty ((== 0) . roomCost) nextCostedRoom
    p2 r = case (roomCost r, catMaybes $ p2 <$> nextRooms r) of
      (0, _) -> Just 0
      (_, []) -> Nothing
      (_, rs) -> Just $ 1 + maximum rs

main :: IO ()
main = readFile "input/day17.txt" >>= print . solve . parse
