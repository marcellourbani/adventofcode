#!/usr/bin/env stack
-- stack --resolver lts-10.2 script
module Main where

import           Data.List.Split

-- >>> solve $ lines "1-3 a: abcde\n 1-3 b: cdefg\n 2-9 c: ccccccccc"
solve :: [String] -> Int
solve = length . filter isValid . fmap parse

type PwdEntry = (String, Int, Int, Char)

-- >>> fmap parse $ lines "1-3 a: abcde\n 1-3 b: cdefg\n 2-9 c: ccccccccc"
-- [("abcde",1,3,'a'),("cdefg",1,3,'b'),("ccccccccc",2,9,'c')]
parse :: String -> PwdEntry
parse s = (pwd,min,max,head char) where
    (limit:char:pwd:_) = words s
    (min:max:_) = read <$> splitOn "-" limit

-- >>> isValid ("abcde",1,3,'a')
-- True
isValid:: PwdEntry -> Bool
isValid (pwd,min,max,char) = count >= min && count <= max where
    count = length $ filter (== char) pwd

main :: IO ()
main = interact $ show.solve.lines
