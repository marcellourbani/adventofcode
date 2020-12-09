#!/usr/bin/env stack
-- stack --resolver lts-10.2 script
module Main where
-- >>> solve 5 "35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576"
-- (127,62)
solve len s = (fst first,second 2) where
  input = read <$> words s
  window s = take len . drop s $ input
  valid i n
    | i < len = True
    | otherwise = not $ null [x|x<-cand,y<-cand,x/=y,x + y == n] where
      cand = window $ i - len
  invalids = [(n,i)|(i,n)<-zip [0..] input,not $ valid i n]
  first = head invalids
  findseq n = [minimum l + maximum l | ofs<-[0..length input - n],let l=take n $ drop ofs input, sum l == fst first ]
  second n
    | n > length input = 0
    | not $ null l  = head l
    | otherwise = second $ n + 1
    where l = findseq n


main :: IO ()
main = readFile "input/day9.txt" >>= print . solve 25

