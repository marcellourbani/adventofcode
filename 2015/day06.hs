#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (foldl', foldr')
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

data Range = Range Int Int Int Int deriving (Show, Eq)

data Operation = On Range | Off Range | Toggle Range deriving (Show, Eq)

parse :: String -> [Operation]
parse i = parseop <$> lines i
  where
    parseop s = case words s of
      ["turn", "off", sr, "through", er] -> Off $ parserange sr er
      ["turn", "on", sr, "through", er] -> On $ parserange sr er
      ["toggle", sr, "through", er] -> Toggle $ parserange sr er
      _ -> error "invalid input"
    parserange f t = uncurry (uncurry Range $ parsexy f) $ parsexy t
      where
        parsexy s = case splitOn "," s of
          [a, b] -> (read a, read b)
          _ -> error "invalid input"

execOp :: M.Map (Int, Int) Bool -> Operation -> M.Map (Int, Int) Bool
execOp m o = M.filter id $ case o of
  On r -> M.union (M.fromList $ zip (rkeys r) (repeat True)) m
  Off r -> M.union (M.fromList $ zip (rkeys r) (repeat False)) m
  Toggle r -> foldr' (M.alter (Just . maybe True not)) m (rkeys r)
  where
    rkeys ((Range x1 y1 x2 y2)) = [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]

execOp2 :: M.Map (Int, Int) Int -> Operation -> M.Map (Int, Int) Int
execOp2 m o = case o of
  On r -> foldr' (addl 1) m $rkeys r
  Off r -> foldr' (M.alter decl) m $ rkeys r
  Toggle r -> foldr' (addl 2) m $rkeys r
  where
    addl i = M.alter (Just . maybe i (+ i))
    decl i = case i of
      Just x | x > 1 -> Just $x -1
      _ -> Nothing
    rkeys ((Range x1 y1 x2 y2)) = [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]

-- >>> solve $ parse "turn on 0,0 through 999,999\ntoggle 0,0 through 999,0\nturn off 499,499 through 500,500"
-- (998996,1001996)

solve :: [Operation] -> (Int, Int)
solve l = (length lights, sum lights2)
  where
    lights = foldl' execOp M.empty l
    lights2 = foldl' execOp2 M.empty l

main :: IO ()
main = readFile "input/day06.txt" >>= print . solve . parse
