#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

parse :: String -> (Int, Int)
parse i = (w !! 15, w !! 17) where w = read <$> words (filter (`notElem` ",.") i)

-- >>> solve $ parse "To continue, please consult the code grid in the manual.  Enter the code at row 2981, column 3075."
-- 9132360

solve :: (Int, Int) -> Int
solve (row, col) = nums !! ind
  where
    nums = iterate next 20151125
    next i = 252533 * i `mod` 33554393
    ind = col -1 + div ((col + row -1) * (col + row - 2)) 2

main :: IO ()
main = readFile "input/day25.txt" >>= print . solve . parse
