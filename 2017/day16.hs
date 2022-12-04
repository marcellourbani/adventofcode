#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Control.Arrow (second)
import Data.Foldable (foldl')
import Data.List (elemIndex)
import Data.List.Split (splitOn)

data Move = Spin Int | Exchange Int Int | Partner Char Char deriving (Show)

-- doesn't handle consumption but don't need it here
instance Read Move where
  readsPrec d s = case s of
    [] -> []
    's' : rest -> [(Spin $read rest, "")]
    'x' : rest -> [(Exchange a b, "")] where [a, b] = read <$> splitOn "/" rest
    'p' : a : '/' : b : _ -> [(Partner a b, "")]
    _ -> undefined

parse :: String -> [Move]
parse s = read <$> splitOn "," s

runMove :: String -> Move -> String
runMove s m = case m of
  Spin n -> b <> a where (a, b) = splitAt (length s - n) s
  Exchange f t | f > t -> runMove s $ Exchange t f
  Exchange f t -> a <> [head rest'] <> tail b <> [head b] <> tail rest'
    where
      (a, rest) = splitAt f s
      (b, rest') = splitAt (t - f) rest
  Partner f t -> case (elemIndex f s, elemIndex t s) of
    (Just fi, Just ti) -> runMove s $ Exchange fi ti
    _ -> s

runProg :: [Move] -> String -> String
runProg p s = foldl' runMove s p

naiveRunMulti :: Int -> [Move] -> String -> String
naiveRunMulti n p s = iterate (runProg p) s !! n

runMulti :: Int -> [Move] -> String -> String
runMulti n p s = go 0 s
  where
    go d c
      | d == n = c
      | c' == s = go d'' c'
      | otherwise = go d' c'
      where
        c' = runProg p c
        d' = d + 1
        d'' = div n d' * d'

-- >>> runMulti 2 (parse "s1,x3/4,pe/b") "abcde"
-- "ceadb"

solve :: [Move] -> (String, String)
solve p = (p1, p2)
  where
    p1 = runProg p ['a' .. 'p']
    p2 = runMulti 1000000000 p ['a' .. 'p']

main :: IO ()
main = readFile "input/day16.txt" >>= print . solve . parse
