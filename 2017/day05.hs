#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import qualified Data.Map.Strict as M

data CPU = CPU Int (M.Map Int Int) deriving (Show, Eq)

parse :: String -> [Int]
parse s = read <$> lines s

next :: (Int -> Int) -> CPU -> CPU
next f c@(CPU pc prog) = case M.lookup pc prog of
  Nothing -> c
  Just off -> CPU (pc + off) $ M.insert pc (f off) prog

-- >>> solve $ parse "0\n3\n0\n1\n-3"
-- (5,10)

solve :: [Int] -> (Int, Int)
solve l = (go (+ 1) initial 0, go p2u initial 0)
  where
    initial = CPU 0 $ M.fromList $ zip [0 ..] l
    p2u o = if o >= 3 then o - 1 else o + 1
    go f c n
      | next f c == c = n
      | otherwise = go f (next f c) (n + 1)

main :: IO ()
main = readFile "input/day05.txt" >>= print . solve . parse
