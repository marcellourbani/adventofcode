#!/usr/bin/env stack
-- stack --resolver lts-10.2 script
module Main where
import           Data.List
import           Numeric
-- >>> seat "BFFFBBFRRR"
-- (70,7,567)
-- seat :: Num c => [Char] -> (c, c, c)
seat :: Num b => [Char] -> (b, (b, b))
seat s = (seat + 8 * line,(line ,seat))
  where line = cvt 'B' . take 7 $ s
        seat = cvt 'R' . drop 7 $ s
        cvt b = fst.head.readInt 2 (const True) (fromEnum.(== b))

solve :: String -> (Integer,Integer)
solve s = (maxSeat,empty) where
    maxSeat = foldr max 0 occupied
    occupied = sort . fmap (fst.seat) . lines $ s
    empty = go occupied
    go (b:c:n)
      | b + 1 < c = b + 1
      | otherwise = go $ c:n
    go _ = 0
main :: IO ()
main = interact $ show.solve
