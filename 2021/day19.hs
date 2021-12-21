-- stack --resolver lts-18.18 script

module Main where

import Data.List.Split (splitOn)
import qualified Data.Set as S

type Point = (Int, Int, Int)

type Input = [[Point]]

parse :: String -> Input
parse s = map pp <$> ls
  where
    ls = tail . lines <$> splitOn "\n\n" s
    pp p = case splitOn "," p of
      [x, y, z] -> (read x, read y, read z)
      _ -> error "parse"

transforms :: Point -> [Point]
transforms a = picks a >>= flips >>= rots
  where
    r90 (x, y, z) = (x, z, - y)
    rots b = take 4 $ iterate r90 b
    rots2 (x, y, z) = [(x, i, j) | i <- [y, - y], j <- [z, - z]]
    flips (x, y, z) = [(x, y, z), (- x, - y, z)]
    picks (x, y, z) = [(x, y, z), (y, x, z), (z, x, y)]

    foo (x, y, z) = (- x, - z, - y)
    bar (x, y, z) = (- x, - z, - y)

comp a b = case (a, b) of
  ([], _) -> []
  (_, []) -> []
  (x : xs, y : ys) -> S.member y (S.fromList (transforms x)) : comp xs ys

-- >>> filter (\(x,_,_) -> x == (-6) ) $ transforms (5,6,-4)
-- [(-6,-5,-4),(-6,-4,5),(-6,5,4),(-6,4,-5)]

-- >>> a = parse "--- scanner 0 ---\n-1,-1,1\n-2,-2,2\n-3,-3,3\n-2,-3,1\n5,6,-4\n8,0,7\n\n--- scanner 0 ---\n1,-1,1\n2,-2,2\n3,-3,3\n2,-1,3\n-5,4,-6\n-8,-7,0\n\n--- scanner 0 ---\n-1,-1,-1\n-2,-2,-2\n-3,-3,-3\n-1,-3,-2\n4,6,5\n-7,0,8\n\n--- scanner 0 ---\n1,1,-1\n2,2,-2\n3,3,-3\n1,3,-2\n-4,-6,5\n7,0,8\n\n--- scanner 0 ---\n1,1,1\n2,2,2\n3,3,3\n3,1,2\n-6,-4,-5\n0,7,-8"
-- >>> b = comp (head a) <$> a
-- >>> all id <$> b
-- >>> b
-- >>> a !! 1
-- [True,True,True,True,False]
-- [[True,True,True,True,True,True],[True,True,True,True,True,True],[True,True,True,True,True,True],[True,True,True,True,True,True],
-- [True,True,True,False,False,True]]
-- [(1,-1,1),(2,-2,2),(3,-3,3),(2,-1,3),(-5,4,-6),(-8,-7,0)]

solve i = i

main :: IO ()
main = readFile "input/day19.txt" >>= print . solve . parse
