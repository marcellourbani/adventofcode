#!/usr/bin/env stack
-- stack --resolver lts-18.18 script --package MissingH

module Main where

import Data.ByteString.Char8 (pack)
import Distribution.Utils.MD5 (md5, showMD5)

md5s :: String -> String
md5s = showMD5 . md5 . pack

-- >>> solve "abcdef"
-- (609043,6742839)

solve :: String -> (Int, Int)
solve l = (p1, go 6 p1)
  where
    p1 = go 5 0
    go t i
      | replicate t '0' == take t h = i
      | otherwise = go t $ i + 1
      where
        h = md5s $ l ++ show i

main :: IO ()
main = readFile "input/day04.txt" >>= print . solve
