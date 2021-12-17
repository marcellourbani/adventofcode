#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List.Split (splitOn)

data Input = Area Int Int Int Int deriving (Show)

data State = State
  { s_x :: Int,
    s_y :: Int,
    s_vx :: Int,
    s_vy :: Int
  }
  deriving (Show)

parse :: String -> Input
parse s = case ls of
  [[minx, maxx], [miny, maxy]] -> Area minx maxx miny maxy
  _ -> error "parse error"
  where
    ls = map read . splitOn ".." . (!! 1) . splitOn "=" <$> splitOn "," t
    t = splitOn ": " s !! 1

next :: State -> State
next (State x y vx vy) = State (x + vx) (y + vy) (drag vx) (grav vy)
  where
    drag v
      | v < 0 = v + 1
      | v > 0 = v -1
      | otherwise = 0
    grav v = v - 1

-- >>> solve  $parse "target area: x=20..30, y=-10..-5"
-- 45

solve :: Input -> Int
solve (Area minx maxx miny maxy) = maxyforv . maximum $ s_vy <$> intercepting
  where
    maxxforv v = div (v * v + v) 2
    maxyforv v = v * v - v * (v -1) `div` 2
    minvx = floor . sqrt $ 2.0 * fromIntegral minx
    maxvx = maxx
    minvy = 0
    maxvy = - miny
    xt v t = v * t' - t' * (t' -1) `div` 2 where t' = min v t
    yt v t = v * t - t * (t -1) `div` 2
    intercepting = filter intersects $ State 0 0 <$> [minvx .. maxvx] <*> [minvy .. maxvy]
    intersects (State x y vx vy)
      | x > maxx || y < miny = False
      | x >= minx && x <= maxx && y >= miny && y <= maxy = True
      | otherwise = intersects $ next (State x y vx vy)

main :: IO ()
main = readFile "input/day17.txt" >>= print . solve . parse
