#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Char (isUpper)
import Data.Foldable (Foldable (foldl', foldr'))
import Data.List (intercalate)
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

nextpaths :: Input -> [String] -> Bool -> S.Set String -> String -> [[String]]
nextpaths i prefix revisited visited current = case (current, found) of
  ("end", _) -> [reverse $ "end" : prefix]
  (_, Nothing) -> []
  (_, Just (isBig, ns)) -> S.toList ends
    where
      -- start,A,b,d,b,end
      isSmall = smallCave isBig current
      revisited' = revisited || (isSmall && S.member current visited)
      visited' = if isBig then visited else S.insert current visited
      alive = if revisited' then S.difference ns visited' else S.delete "start" ns
      ends = S.unions $ S.fromList . nextpaths i (current : prefix) revisited' visited' <$> S.toList alive
  where
    found = M.lookup current i
    limits = S.fromList ["start", "end"]
    smallCave isBig v = not $ isBig && S.member v limits

-- >>> solve $ parse "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end"
-- (10,36)

-- >>> solve  $parse "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc"
-- (19,103)

solve :: Input -> (Int, Int)
solve m = (length paths, length paths2)
  where
    paths = nextpaths m [] True S.empty "start"
    paths2 = nextpaths m [] False S.empty "start"

main :: IO ()
main = readFile "input/day12.txt" >>= print . solve . parse
