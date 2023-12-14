#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import Data.Foldable (find)
import Data.List.Split (splitOn)
import qualified Data.Set as S

data Grid = Grid {gWidth :: Int, gHeight :: Int, gGrid :: S.Set (Int, Int)} deriving (Show)

parse :: String -> [Grid]
parse g = parseGrid <$> splitOn "\n\n" g
  where
    parseGrid g = Grid w h $ S.fromList [(x, y) | (y, l) <- zip [1 ..] ls, (x, c) <- zip [1 ..] l, c == '#']
      where
        ls = lines g
        w = length $ head ls
        h = length ls

findRreflection :: Bool -> Grid -> Int
findRreflection smudged (Grid w h g) = case (vref, href) of
  (Just l, Just r) | r < l -> r
  (Just l, _) -> 100 * l
  (_, Just r) -> r
  _ -> 0
  where
    vref = find (findrefl smudged lines) [1 .. h - 1]
    href = find (findrefl smudged columns) [1 .. w - 1]
    line y = S.map fst $ S.filter ((== y) . snd) g
    column x = S.map snd $ S.filter ((== x) . fst) g
    lines = line <$> [1 .. h]
    columns = column <$> [1 .. w]
    smudgeCompare seen l = case l of
      [] -> seen
      (a, b) : xs
        | a == b -> smudgeCompare seen xs
        | seen -> False
        | d == 1 -> smudgeCompare True xs
        | otherwise -> False
        where
          d = sum $ S.size <$> [S.difference a b, S.difference b a]
    findrefl sm xs n
      | sm = smudgeCompare False $ zip l' $ reverse r'
      | otherwise = l' == reverse r'
      where
        (l, r) = splitAt n xs
        [lenl, lenr] = length <$> [l, r]
        minl = min lenl lenr
        l' = drop (lenl - minl) l
        r' = take minl r

-- >>> solve $ parse "#.##..##.\n..#.##.#.\n##......#\n##......#\n..#.##.#.\n..##..##.\n#.#.##.#.\n\n#...##..#\n#....#..#\n..##..###\n#####.##.\n#####.##.\n..##..###\n#....#..#"
-- (405,400)

solve :: [Grid] -> (Int, Int)
solve l = (p1, p2)
  where
    p1 = sum $ findRreflection False <$> l
    p2 = sum $ findRreflection True <$> l

main :: IO ()
main = readFile "input/day13.txt" >>= print . solve . parse
