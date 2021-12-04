#!/usr/bin/env stack
-- stack --resolver lts-12.21 script

{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad (foldM_, zipWithM_)
import Control.Monad.ST
import Data.List.Split (splitOn)
import qualified Data.Vector.Storable.Mutable as V

-- mostly copied from https://github.com/haskelling/aoc2020/blob/main/23b.hs
-- changed enough to make sure I understand it
game2 :: [Int] -> Int -> Int -> Int
game2 xs n nIters = runST $ do
  v <- V.new n -- vector of tuples (idx,value)
  zipWithM_ (V.write v) xs' (tail $ cycle xs') -- sets each cell in the vector to the next value, last oe to the 1s value
  -- this way the vector implements a (circular) linked list
  foldM_ (const . step2 v) (head xs') [1 .. nIters]
  a <- V.read v 0
  b <- V.read v a
  return $ (a + 1) * (b + 1)
  where
    xs' = map (+ (-1)) xs ++ [length xs .. n -1] -- (original list plus length l to n - 1), all decreased by one
    dec :: Int -> Int -- circular decrease
    dec 0 = n - 1
    dec x = x - 1
    step2 v current = do
      x1 <- V.read v current
      x2 <- V.read v x1
      x3 <- V.read v x2
      next <- V.read v x3
      let dec' x0 = if x0 == x1 || x0 == x2 || x0 == x3 then dec' $ dec x0 else x0
          x = dec' $ dec current -- current cell
      V.read v x >>= V.write v x3 -- point x3 to after x
      V.write v x x1 -- point x to x1 (now x->x1->x2->x3)
      V.write v current next -- point current -> next
      return next

-- >>> solve "389125467"
-- ("67384529",149245887792)

solve :: String -> (String, Int)
solve s = (first, second)
  where
    first = concat $ show <$> game input 100
    second = game2 input 1000000 10000000
    input :: [Int]
    input = read . (: []) <$> s
    maxi = maximum input
    game :: [Int] -> Int -> [Int]
    game !i sn
      | sn == 0 = pos ++ pre
      | otherwise = game (step i) $ sn -1
      where
        [pre, pos] = splitOn [1] i
    step [] = []
    step (x : xs) = pre ++ (next : cur ++ pos ++ [x])
      where
        cur = take 3 xs
        next = head [nx | i <- [1 .. maxi], let nx = 1 + mod (x - i -1) maxi, nx `notElem` cur]
        [pre, pos] = splitOn [next] $ drop 3 xs

main :: IO ()
main = print $ solve "362981754"
