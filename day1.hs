#!/usr/bin/env stack
-- stack --resolver lts-10.2 script
module Main where
import           Data.List (tails)
-- >>> solve [1721,979,366,299,675,1456] -- (514579,241861950)
-- (514579,241861950)
solve:: [Int] -> ( Int, Int)
solve l = (a*b ,c*d*e) where
    (a,b) = head couples
    (c,d,e) = head triplets
    t = tail $ tails l
    couples = [(x,y)| (x,r)<-zip l t,y<-r, x+y == 2020]
    triplets = [(x,y,z)|(x,r1,r2)<- zip3 l t (tail t),y<-r1,z<-r2,x+y+z == 2020]

main :: IO ()
main = readFile "input/day1.txt" >>= print.solve.fmap read. words
