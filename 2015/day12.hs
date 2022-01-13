#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import qualified Data.Aeson.Types as T
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import Data.Text (Text)

parse :: B.ByteString -> Value
parse i = fromMaybe Null $ decode i

sumNumbers :: Value -> Int
sumNumbers v = case v of
  Object o -> sum $ sumNumbers <$> o
  Array a -> sum $ sumNumbers <$> a
  Number n -> floor n
  _ -> 0

sumNumbers2 :: Value -> Int
sumNumbers2 v = case v of
  Object o | String "red" `notElem` o -> sum $ sumNumbers2 <$> o
  Array a -> sum $ sumNumbers2 <$> a
  Number n -> floor n
  _ -> 0

-- >>> solve $ parse "{\"a\":1, \"b\":2,\"r\":\"red\"}"
-- (3,0)

solve l = (sumNumbers l, sumNumbers2 l)

main :: IO ()
main = B.readFile "input/day12.txt" >>= print . solve . parse
