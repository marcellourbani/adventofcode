#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)

parse :: B.ByteString -> Value
parse i = fromMaybe Null $ decode i

sumNumbers :: (Object -> Bool) -> Value -> Int
sumNumbers f v = case v of
  Object o | f o -> sum $ sumNumbers f <$> o
  Array a -> sum $ sumNumbers f <$> a
  Number n -> floor n
  _ -> 0

-- >>> solve $ parse "{\"a\":1, \"b\":2,\"r\":\"red\"}"
-- (3,0)
solve :: Value -> (Int, Int)
solve l = (sumNumbers (const True) l, sumNumbers noRed l) where noRed o = String "red" `notElem` o

main :: IO ()
main = B.readFile "input/day12.txt" >>= print . solve . parse
