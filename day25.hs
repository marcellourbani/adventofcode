#!/usr/bin/env stack
-- stack --resolver lts-12.21 script

{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

decode :: Int -> Int
decode x = y
  where
    transforms = go 1 where go n = n : go (mod (7 * n) 20201227)
    y = fromMaybe 0 $elemIndex x transforms

modinv :: Integral a => a -> a -> a
modinv n m = mod inv m
  where
    (_, inv, _) = ee n m
    ee 0 b = (b, 0, 1)
    ee a b = (g, x - (div b a * y), y) where (g, y, x) = ee (mod b a) a

-- >>> solve "5764801\n17807724"
-- (14897079,11)

solve :: String -> (Int, Int)
solve s = (first, second)
  where
    first = ek
    second = cardls
    ek = encode cardls doorpk
    cardls = findloopsize cardpk
    [doorpk, cardpk] = read <$> lines s
    encode k subj = transform subj !! k
    transform subj = go 1 where go n = n : go (mod (subj * n) 20201227)
    transforms = transform 7
    findloopsize x = fromMaybe 0 $elemIndex x transforms

main :: IO ()
main = readFile "input/day25.txt" >>= print . solve