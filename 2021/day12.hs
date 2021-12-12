#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Char (isUpper)
import Data.Foldable (Foldable (foldl', foldr'))
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Tuple (swap)

type Input = M.Map String (Bool, S.Set String)

parse :: String -> Input
parse s = m
  where
    b = (\[a, b] -> (a, b)) . splitOn "-" <$> lines s
    addpath m (a, b) = M.insert a n m
      where
        n = case M.lookup a m of
          Nothing -> (isUpper $ head a, S.fromList [b])
          (Just (c, s)) -> (c, S.insert b s)
    m = foldl' addpath M.empty $ b ++ (swap <$> b)

nextpaths :: Input -> [String] -> S.Set String -> String -> [[String]]
nextpaths i prefix visited current = case (current, found) of
  ("end", _) -> [reverse $ "end" : prefix]
  (_, Nothing) -> []
  (_, Just (caps, ns)) -> mconcat ends
    where
      visited' = if caps then visited else S.insert current visited
      alive = S.difference ns visited'
      ends = nextpaths i (current : prefix) visited' <$> S.toList alive
  where
    found = M.lookup current i

-- >>> solve $ parse "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end"
-- 10

solve :: Input -> (Int, Int)
solve m = (length paths, 0)
  where
    paths = nextpaths m [] S.empty "start"

main :: IO ()
main = readFile "input/day12.txt" >>= print . solve . parse
