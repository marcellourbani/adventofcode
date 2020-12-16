#!/usr/bin/env stack
-- stack --resolver lts-10.2 script
{-# LANGUAGE TupleSections #-}
module Main where
import           Data.List.Split
-- >>> solve "class: 1-3 or 5-7\nrow: 6-11 or 33-44\nseat: 13-40 or 45-50\n\nyour ticket:\n7,1,14\n\nnearby tickets:\n7,3,47\n40,4,50\n55,2,20\n38,6,12"
-- (71,2)

-- solve :: [Char] -> (Int, Int)
solve s = (first ,second) where
  first = sum $ sum . invalid <$> tickets
  second = 2
  (myticket,tickets,constraints) = parse . fmap (splitOn ":") . filter (/="") . lines $ s
  checkcon n c = or $ (\(min,max) -> n>=min && n <= max) <$> c
  invalid t = [ x | x <-t ,not $ or $ checkcon x <$> fmap snd constraints]
  parse ls = ( mytycket, others,fields) where
    (rawfields:tmp:_) = splitOn [["your ticket",""]] ls
    ([[rawmy]]:rawothers:_) = splitOn [["nearby tickets",""]] tmp
    parseField:: [String] -> (String,[(Int,Int)])
    parseField  f = (head f,) $ fmap (\(x:y:_)->(x,y)) $ fmap read . splitOn "-" <$> splitOn " or " ( f !!1)
    fields = parseField <$> rawfields
    parseTicket :: String -> [Int]
    parseTicket = fmap read . splitOn ","
    mytycket = parseTicket rawmy
    others = parseTicket . head <$> rawothers


main :: IO ()
main = readFile "input/day16.txt" >>= print.solve
