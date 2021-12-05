#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Array

data Seat = Floor | Empty | Occupied deriving (Eq, Show)

decode :: Char -> Seat
decode 'L' = Empty
decode '#' = Occupied
decode _ = Floor

encode :: Seat -> Char
encode Empty = 'L'
encode Occupied = '#'
encode Floor = '.'

data World = World {wcolumns :: Int, wrows :: Int, wseats :: Array Int (Array Int Seat)}
  deriving (Eq)

instance Show World where
  show (World _ _ l) = unlines $ elems $ elems . fmap encode <$> l

inWRange :: World -> Int -> Int -> Bool
inWRange (World numcols numrows _) x y = and [x >= 0, x < numcols, y >= 0, y < numrows]

seatAt :: World -> Int -> Int -> Seat
seatAt w x y
  | inWRange w x y = wseats w ! y ! x
  | otherwise = Floor

inSight :: World -> Int -> Int -> Int
inSight world x y = hits
  where
    speeds = [(vx, vy) | vx <- [-1 .. 1], vy <- [-1 .. 1], (vx, vy) /= (0, 0)]
    hits = sum $ hit (x, y) <$> speeds
    nextPos (px, py) (vx, vy) = (px + vx, py + vy)
    hit p v
      | inr = case seatAt world nx ny of
        Occupied -> 1
        Empty -> 0
        _ -> hit (nx, ny) v
      | otherwise = 0
      where
        (nx, ny) = nextPos p v
        inr = inWRange world nx ny

adjacents :: World -> Int -> Int -> Int
adjacents world x y = k
  where
    k = length [s | c <- [x -1 .. x + 1], l <- [y -1 .. y + 1], let s = seatAt world c l, s == Occupied, (c, l) /= (x, y)]

nextSeat :: Int -> (World -> Int -> Int -> Int) -> World -> Int -> Int -> Seat
nextSeat maxn calc w x y = case seatAt w x y of
  Floor -> Floor
  Empty
    | n == 0 -> Occupied
    | otherwise -> Empty
  Occupied
    | n >= maxn -> Empty
    | otherwise -> Occupied
  where
    n = calc w x y

-- >>> solve "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL"
-- (37,26)
solve :: String -> (Int, Int)
solve s = (first, total final2)
  where
    first = total final
    parseLine = fmap decode
    world = createWorld $ parseLine <$> lines s
    final = stationary 4 adjacents world
    final2 = stationary 5 inSight world
    total w = sum $ sum . fmap (\seat -> if seat == Occupied then 1 else 0) <$> wseats w

stationary :: Int -> (World -> Int -> Int -> Int) -> World -> World
stationary maxn calc w
  | w == nw = w
  | otherwise = stationary maxn calc nw
  where
    nw = nextWorld maxn calc w

nextWorld :: Int -> (World -> Int -> Int -> Int) -> World -> World
nextWorld maxn calc w = createWorld nw
  where
    numcols = wcolumns w
    numrows = wrows w
    nl y = [s | x <- [0 .. numcols -1], let s = nextSeat maxn calc w x y]
    nw = [l | y <- [0 .. numrows -1], let l = nl y]

createWorld :: [[Seat]] -> World
createWorld s = World cols rows seats
  where
    rows = length s
    cols = length . head $ s
    seats = listArray (0, rows - 1) [r | l <- s, let r = listArray (0, cols -1) l]

main :: IO ()
main = readFile "input/day11.txt" >>= print . solve
