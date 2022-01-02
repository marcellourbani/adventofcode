-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (find)
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
    flips (x, y, z) = [(x, y, z), (- x, y, - z)]
    picks (x, y, z) = [(x, y, z), (y, - x, z), (z, y, - x)]

solve i = i

main :: IO ()
main = readFile "input/day19.txt" >>= print . solve . parse
