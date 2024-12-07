#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

{-# LANGUAGE PackageImports #-}

module Main where

import Data.List.Split (splitOn)
import "containers" Data.Map.Strict qualified as M
import "containers" Data.Set qualified as S

data Input = Input {iPrec :: [(Int, Int)], iUpd :: [[Int]]} deriving (Show)

parse :: String -> Input
parse s = Input (ptup <$> p1) (map read . splitOn "," <$> p2)
  where
    [p1, p2] = lines <$> splitOn "\n\n" s
    ptup a = (x, y) where [x, y] = read <$> splitOn "|" a

precMap :: Input -> M.Map Int (S.Set Int)
precMap (Input ps _) = M.unionsWith S.union [M.singleton b $ S.singleton a | (a, b) <- ps]

part1 :: Input -> Int
part1 i = sum $ mid <$> valids
  where
    pmap = precMap i
    mid l = l !! div (length l - 1) 2
    valids = filter (valid S.empty) $ iUpd i
    valid bl l = case l of
      [] -> True
      x : xs
        | S.member x bl -> False
        | otherwise -> valid bl' xs
        where
          bl' = S.union bl $ M.findWithDefault S.empty x pmap

-- part2 :: Input -> Int
part2 l = 0

-- >>> solve $ parse "47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|13\n\n75,47,61,53,29\n97,61,53,29,13\n75,29,13\n75,97,47,61,53\n61,13,29\n97,13,75,29,47"
-- (143,0)

solve l = (p1, p2)
  where
    p1 = part1 l
    p2 = part2 l

main :: IO ()
main = readFile "input/day05.txt" >>= print . solve . parse
