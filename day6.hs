#!/usr/bin/env stack
-- stack --resolver lts-10.2 script
module Main where
import           Data.List.Split
import           Data.Set        (empty, fromList, intersection, union)
-- >>> solve "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb"
-- (11,6)
solve :: String -> (Int,Int)
solve s = (first sets , second sets)
  where sets = parse s
        first = sum . fmap  (length.foldr union empty)
        second = sum . fmap  (length.foldr intersection (fromList ['a'..'z']))
        parse = fmap (fmap fromList . lines) . splitOn "\n\n"

main :: IO ()
main = readFile "input/day6.txt" >>= print.solve
