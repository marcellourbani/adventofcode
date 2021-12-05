#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List.NonEmpty (NonEmpty ((:|)), xor)
import Data.List.Split

-- >>> solve $ lines "1-3 a: abcde\n 1-3 b: cdefg\n 2-9 c: ccccccccc"
-- (2,1)
solve :: [String] -> (Int, Int)
solve l = (val1, val2)
  where
    val1 = length . filter isValid $ candidates
    val2 = length . filter isValid2 $ candidates
    candidates = parse <$> l

type PwdEntry = (String, Int, Int, Char)

-- >>> fmap parse $ lines "1-3 a: abcde\n 1-3 b: cdefg\n 2-9 c: ccccccccc"
-- [("abcde",1,3,'a'),("cdefg",1,3,'b'),("ccccccccc",2,9,'c')]
parse :: String -> PwdEntry
parse s = (pwd, min, max, head char)
  where
    (limit : char : pwd : _) = words s
    (min : max : _) = read <$> splitOn "-" limit

-- >>> isValid ("abcde",1,3,'a')
-- True
isValid :: PwdEntry -> Bool
isValid (pwd, min, max, char) = count >= min && count <= max
  where
    count = length $ filter (== char) pwd

-- >>> isValid2 ("abcde",1,3,'a')
-- True
isValid2 :: PwdEntry -> Bool
isValid2 (pwd, min, max, char) = xor (foundFirst :| [foundSecond])
  where
    foundFirst = pwd !! (min -1) == char
    foundSecond = pwd !! (max -1) == char

main :: IO ()
main = interact $ show . solve . lines
