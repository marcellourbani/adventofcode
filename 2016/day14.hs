#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.ByteString.Char8 (pack)
import Data.List (isInfixOf)
import Distribution.Utils.MD5 (md5, showMD5)

parse = id

md5s :: String -> String
md5s = showMD5 . md5 . pack

-- >>> solve "abc"
-- (22728,22551)

solve :: String -> (Int, Int)
solve l = (go 64 candidates, go 64 candidates2)
  where
    candidates = zip [0 ..] $ md5s . (l ++) . show <$> [0 ..]
    candidates2 = zip [0 ..] $ stretched . (l ++) . show <$> [0 ..]
    stretched s = iterate md5s s !! 2017
    triplet s = case s of
      a : b : c : _ | a == b && a == c -> Just a
      x : xs -> triplet xs
      [] -> Nothing
    go n ms = case ms of
      [] -> error "impossible"
      x : xs -> case triplet $ snd x of
        Nothing -> go n xs
        Just c -> case (isvalid, n) of
          (True, 1) -> fst x
          (True, n) -> go (n -1) xs
          _ -> go n xs
          where
            isvalid = any (replicate 5 c `isInfixOf`) (take 1000 $ snd <$> xs)

main :: IO ()
main = readFile "input/day14.txt" >>= print . solve . parse
