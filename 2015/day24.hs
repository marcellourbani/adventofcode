#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

parse :: String -> [Int]
parse i = read <$> lines i

-- >>> solve $ parse "1\n2\n3\n4\n5\n7\n8\n9\n10\n11"
-- (99,44)

solve :: [Int] -> (Int, Int)
solve l = (s targ, s targ2)
  where
    s t = minimum $ product <$> shortest t 1
    targ = div (sum l) 3
    targ2 = div (sum l) 4
    shortest t n = case sumto t n l of
      [] -> shortest t $n + 1
      xs -> xs
    sumto t n l = case (l, n) of
      ([], _) -> []
      (_, 0) -> []
      (x : xs, _)
        | x == t -> [x] : nexts
        | x > t -> nexts
        | otherwise -> ((x :) <$> sumto (t - x) (n -1) xs) ++ nexts
        where
          nexts = sumto t n xs

main :: IO ()
main = readFile "input/day24.txt" >>= print . solve . parse
