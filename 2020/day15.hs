#!/usr/bin/env stack
-- stack --resolver lts-10.2 script
{-# LANGUAGE TupleSections #-}
module Main where

import           Data.List       (mapAccumL)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)

data State = State {sindex::Int, sviews:: M.Map Int [Int],slast::Int,sstartup::[Int] } deriving Show

delta :: [Int]->Int
delta (l:p:_) =  l - p
delta _       = 0

-- my own solution, working but a bit slow and not very elegant
step:: State -> State
step (State idx views lastnum strtnums) = State (idx + 1) nextview i strtnums
  where spklen = length strtnums
        i = if idx <= spklen then strtnums !! (idx - 1)
            else maybe 0  delta $ views M.!? lastnum
        prevview =  fromMaybe [] $ views M.!? i
        nextview = M.insert i (take 2 $ idx:prevview) views

-- van eck copied from web, adapted with starting list
myVanEck :: [Int] -> [Int]
myVanEck l = l ++ snd (mapAccumL go (last l, initial) [length l-1 ..])
  where
    initial = M.fromList $ zip l [0..]
    go (x, dct) i =
      ((,) =<< (, M.insert x i dct)) (maybe 0 (i -) (M.lookup x dct))



-- >>> solve [0,3,6]
-- (436,2)

solve :: [Int] -> (Int, Int)
solve l = (first,second) where
  first = slast $ go 2020 initst
  second = veSolve 30000000
  veSolve i = myVanEck l !! (i - 1)

  go n s
    | n == 0 = s
    | otherwise = go (n-1) $ step s

  initst = State 1 M.empty 0 l


main :: IO ()
main = print $ solve [19,0,5,1,10,13]
