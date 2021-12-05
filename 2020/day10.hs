#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
module Main where
import           Data.List

removeAt i l = take i l ++ drop (i+1) l

validSublists l = uniq . sort $ l : go123 l where
  go123 l
    | null sub = []
    | otherwise = sub ++ concat (validSublists <$> sub)
    where sub = validSublists1 l
  validSublists1 l  = v where
    indexes = [1..length l - 2]
    v = [ v1 |i <- indexes,let v1 = removeAt i l,valid v1]
  uniq l = case l of
    [] -> []
    [_] -> l
    (x:y:xs) 
      | x == y -> uniq (x:xs)
      | otherwise -> x : uniq (y:xs)

valid x = case x of
  []-> True
  [_] -> True
  (x1:x2:xs) -> (x2 - x1) <= 3 && valid (x2:xs)
    
-- >>> solve "28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3"
-- (220,19208)

solve :: [Char] -> (Int, Int)
solve s = (first ,product $ length.validSublists <$> splits adapters)
-- solve s = (first ,deltas)
  where adapters = 0: a ++ [maximum a + 3]
        a = sort $ fmap read . words $ s
        deltas = [y-x|(x,y)<-zip adapters $ drop 1 adapters]
        count n = length . filter (==n) $ deltas
        first = count 1 * count 3
        breaks a = 0:[n+1|(x1,x2,n)<- zip3 a (drop 1 a) [0..],x2-x1 >= 3 ]
        splits a = [x | (f:t:_)<-tails $ breaks a,let x = take (t - f + 1) $ drop f a]




main :: IO ()
main = readFile "input/day10.txt" >>= print.solve
