#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Maybe (fromMaybe)

parse :: String -> [(Int, Int)]
parse s = pl <$> lines (filter (/= '.') s)
  where
    pl l = (read $last w, read $w !! 3) where w = words l

modularinverse :: Int -> Int -> Maybe Int
modularinverse a m = go 0 1 m a
  where
    go t t1 r r1 = case r1 of
      0
        | r > 1 -> Nothing
        | otherwise -> Just $ mod t m
      _ -> go t1 t2 r1 r2
        where
          q = div r r1
          t2 = t - q * t1
          r2 = r - q * r1

chineseRemainder :: [(Int, Int)] -> Int
chineseRemainder l = mod (sum $ term <$> l) pbases
  where
    pbases = product $ snd <$> l
    term (rem, base) = case modularinverse b base of
      Nothing -> error "modularinverse failed"
      Just b' -> rem * b' * b
      where
        b = div pbases base

solve :: [(Int, Int)] -> (Int, Int)
solve l = (p1, p2)
  where
    calcremainder (t, (res, bas)) = (mod (bas - res - t) bas, bas)
    p1 = chineseRemainder $ calcremainder <$> zip [1 ..] l
    p2 = chineseRemainder $ calcremainder <$> zip [1 ..] (l <> [(0, 11)])

main :: IO ()
main = readFile "input/day15.txt" >>= print . solve . parse
