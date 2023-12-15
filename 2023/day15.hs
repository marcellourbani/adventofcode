#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

type Boxes = M.Map Int [(String, Int)]

parse :: String -> [String]
parse = splitOn ","

hash :: String -> Int
hash = go 0
  where
    go n l = case l of
      [] -> n
      x : xs -> go (((fromEnum x + n) * 17) `mod` 256) xs

processBoxes :: [String] -> Boxes
processBoxes = go M.empty
  where
    splitop s = (a, tail b) where (a, b) = break (`elem` "-=") s
    removeLabel lid l = case l of
      [] -> []
      x : xs | fst x == lid -> xs
      x : xs -> x : removeLabel lid xs
    updateLabel lid n l = case (l, lookup lid <$> l) of
      (Nothing, _) -> [(lid, n)]
      (Just oth, Nothing) -> (lid, n) : oth
      (Just old, Just x) -> a <> ((lid, n) : drop 1 b) where (a, b) = break ((== lid) . fst) old
    go bs l = case l of
      [] -> bs
      x : xs -> case splitop x of
        (l, "") -> go (M.update (Just . removeLabel l) (hash l) bs) xs
        (l, n) -> go (M.alter (Just . updateLabel l (read n)) (hash l) bs) xs

focusPower' :: [(String, Int)] -> Int
focusPower' l = sum $ uncurry (*) <$> zip [1 ..] (snd <$> l)

focusPower :: Boxes -> Int
focusPower b = sum $ lp <$> M.toList b
  where
    a = lp <$> M.toList b
    lp (k, line) = (k + 1) * sum (uncurry (*) <$> zip [1 ..] (snd <$> line))

-- >>> solve $ parse "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
-- (1320,145)

solve :: [String] -> (Int, Int)
solve l = (p1, p2)
  where
    p1 = sum $ hash <$> l
    p2 = focusPower $ processBoxes l

main :: IO ()
main = readFile "input/day15.txt" >>= print . solve . parse
