#!/usr/bin/env stack
-- stack --resolver lts-10.2 script
module Main where
import           Data.Array

data Seat = Floor | Empty | Occupied deriving (Eq,Show)
decode :: Char -> Seat
decode 'L' = Empty
decode '#' = Occupied
decode _   = Floor

encode :: Seat -> Char
encode Empty    = 'L'
encode Occupied = '#'
encode Floor    = '.'

data World = World {wcolumns :: Int, wrows :: Int, wseats :: Array Int (Array Int Seat)}
  deriving (Eq)

instance Show World where
  show (World _ _ l) = unlines $ elems $ elems . fmap encode <$> l


seatAt :: World -> Int -> Int -> Seat
seatAt (World numcols numrows seats) x y
  | x < 0 || x >= numcols ||
    y < 0 || y >= numrows = Floor
  | otherwise = seats ! y ! x

adjacents :: World -> Int -> Int -> Int
adjacents world x y = k
  where k = length [s | c <- [x-1..x+1], l <- [y-1..y+1],let s = seatAt world c l,s == Occupied ,(c,l) /= (x,y)]

nextSeat :: World -> Int -> Int -> Seat
nextSeat w x y = case seatAt w x y of
  Floor -> Floor
  Empty
    | n == 0 -> Occupied
    | otherwise -> Empty
  Occupied
    | n >= 4 -> Empty
    | otherwise -> Occupied
  where n = adjacents w x y

-- >>> solve "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL"
-- (37,2)

solve :: [Char] -> (Int,Int)
solve s = (first ,2  )
  where first = total final
        parseLine = fmap decode
        world = createWorld $ parseLine <$> lines s
        final = stationary world
        total w = sum $ sum.fmap (\seat -> if seat == Occupied then 1 else 0) <$> wseats w

stationary :: World -> World
stationary w
  | w == nw = w
  | otherwise = stationary nw
  where nw = nextWorld w

nextWorld :: World -> World
nextWorld w = createWorld nw where
  numcols = wcolumns w
  numrows = wrows w
  nl y = [s|x<-[0..numcols-1],let s = nextSeat w x y ]
  nw = [l| y<-[0..numrows-1],let l = nl y]

createWorld :: [[Seat]] -> World
createWorld s = World cols rows seats
  where rows = length s
        cols = length . head $ s
        seats = listArray (0,rows - 1) [r|l<-s,let r = listArray (0,cols -1) l ]

main :: IO ()
main = readFile "input/day11.txt" >>= print.solve
