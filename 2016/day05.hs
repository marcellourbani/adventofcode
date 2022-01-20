#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.ByteString.Char8 (pack)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as M
import Distribution.Utils.MD5 (md5, showMD5)

parse s = s

md5s :: String -> String
md5s = showMD5 . md5 . pack

-- >>> solve $ parse "abc"
-- ("18f47a30","05ace8e3")

solve :: String -> (String, String)
solve l = (go 8 0 "", go2 8 0 M.empty)
  where
    pref = "00000"
    go c n acc = case (c, pref `isPrefixOf` hash) of
      (0, _) -> reverse acc
      (_, True) -> go (c -1) (n + 1) (hash !! 5 : acc)
      _ -> go c (n + 1) acc
      where
        hash = md5s $ l ++ show n
    go2 c n acc = case (c == length acc, pref `isPrefixOf` hash) of
      (True, _) -> snd <$> M.toList acc
      (_, True) -> go2 c n' acc'
      _ -> go2 c n' acc
      where
        n' = n + 1
        hash = md5s $ l ++ show n
        k = hash !! 5
        acc' = if k `elem` "01234567" then acc <> M.singleton k (hash !! 6) else acc

main :: IO ()
main = readFile "input/day05.txt" >>= print . solve . parse
