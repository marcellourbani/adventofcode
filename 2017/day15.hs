#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Bits ((.&.))

parse :: String -> (Int, Int)
parse s = (read a, read b)
  where
    [la, lb] = lines s
    a = words la !! 4
    b = words lb !! 4

test :: Int -> Int -> Bool
test a b = (a .&. 65535) == (b .&. 65535)

next1 :: Int -> Int -> Int
next1 f c = f * c `mod` 2147483647

next2 :: (Int -> Int) -> Int -> Int -> Int
next2 f d c = case n `mod` d of
  0 -> n
  _ -> next2 f d n
  where
    n = f c

countMatches :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Int -> Int -> Int
countMatches fa fb ca cb n count
  | n == 0 = count
  | test ca cb = countMatches fa fb (fa ca) (fb cb) (n -1) $ count + 1
  | otherwise = countMatches fa fb (fa ca) (fb cb) (n -1) count

-- >>> solve (65,8921)
-- (588,309)

solve :: (Int, Int) -> (Int, Int)
solve (sa, sb) = (p1, p2)
  where
    fa = next1 16807
    fb = next1 48271
    p1 = countMatches fa fb sa sb 40000000 0
    fa2 = next2 fa 4
    fb2 = next2 fb 8
    p2 = countMatches fa2 fb2 sa sb 5000000 0

main :: IO ()
main = readFile "input/day15.txt" >>= print . solve . parse
