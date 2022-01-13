#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

data Ingredient = Ingredient
  { iname :: String,
    icapacity :: Int,
    idurability :: Int,
    iflavor :: Int,
    itexture :: Int,
    icalories :: Int
  }
  deriving (Show, Eq)

parse :: String -> [Ingredient]
parse s = pl . words <$> lines (filter (/= ',') s)
  where
    pl w = Ingredient (head w) (read $ w !! 2) (read $ w !! 4) (read $ w !! 6) (read $ w !! 8) (read $ w !! 10)

score :: [(Ingredient, Int)] -> Int
score il = product $ totl <$> [icapacity, idurability, iflavor, itexture]
  where
    tot f (i, c) = f i * c
    totl f = max 0 $ sum $ tot f <$> il

score2 :: Int -> [(Ingredient, Int)] -> Int
score2 targ il = if tcal == targ then score il else 0
  where
    tcal = sum $uncurry (*) <$> zip (icalories . fst <$> il) (snd <$> il)

-- >>> solve $ parse "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8\nCinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"
-- (62842880,57600000)

solve :: [Ingredient] -> (Int, Int)
solve il = (go il 100 [] score, go il 100 [] $score2 500)
  where
    go l m acc f = case l of
      [] -> 0
      [i] -> f $ (i, m) : acc
      (i : is) -> maximum $ partial <$> [0 .. m]
        where
          partial n = go is (m - n) ((i, n) : acc) f

main :: IO ()
main = readFile "input/day15.txt" >>= print . solve . parse
