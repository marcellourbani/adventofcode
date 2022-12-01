#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where
import           Control.Lens    (re, (^.))
import           Data.Bits       (xor)
import           Data.Char       (chr)
import           Data.Foldable   (Foldable (foldl'))
import           Data.List.Split (splitOn)
import qualified Data.Set        as S
import           GHC.Base        (ord)
import           Numeric.Lens    (binary, hex)

data State = State {slist :: [Int], sindex :: Int, sskip :: Int} deriving (Show, Eq)


knothash :: Int -> String -> [Int]
knothash n s = reduce sparse  -- >>= tohex
  where
    initial = State [0 .. n] 0 0
    sparse = slist $ foldl' move initial l2'
    base = (ord <$> s) <> [17, 31, 73, 47, 23]
    l2' = take (64 * length base) $ cycle base
    reduce l = case take 16 l of
      [] -> []
      l' -> foldl' xor 0 l' : reduce (drop 16 l)
    move (State l i s) n = State l''' i' (s + 1)
      where
        ll = length l
        i' = mod (i + n + s) ll
        l' = drop i l ++ take i l
        l'' = reverse (take n l') ++ drop n l'
        l''' = drop (ll - i) l'' ++ take (ll - i) l''

tohex :: Int -> String
tohex v
  | v >= 16 = v ^. re hex
  | otherwise = '0' : v ^. re hex

tobin :: Int -> String
tobin v
  | l < 8 = replicate (8-l) '0' <> bin
  | otherwise = bin
  where bin = v ^. re binary
        l = length bin

findRegions:: [[Int]]->[S.Set (Int,Int)]
findRegions lines = getRegions fullSet
 where
  fullSet = S.fromList [(y,x) | (l,y) <-zip lines [0..],(v,x)<-zip l [0..],v==1 ]
  ne x = [y | y<- [x-1,x+1],y /= -1,y /= 128]
  adjacent (x,y) = zip [x,x] ( ne y) <> zip (ne x) [y,y]
  getRegions nodes
    | S.null nodes = []
    | otherwise = region : getRegions rest
      where
        seed = S.take 1 nodes
        (rest,region) = getRegion (S.difference nodes seed) seed
  getRegion nodes zone
      | S.null added || S.null nodes = (newnodes,zone)
      | otherwise = getRegion newnodes newzone
    where
      neighbors = S.intersection nodes $ S.fromList $ S.toList zone >>= adjacent
      added = S.difference neighbors zone
      newzone = S.union zone added
      newnodes = S.difference nodes newzone



-- >>> solve "flqrgnkx"
-- (8108,1242)

solve :: [Char] -> (Int,Int)
solve n = (p1,p2)
  where
    p1 = sum $ sum <$> hashes
    p2 = length $ findRegions hashes
    prefix = n <> "-"
    basetexts = (prefix <>) .  show <$> [0..127]
    toline b = read.pure <$> (knothash 255 b >>= tobin)
    hashes = toline <$> basetexts

main :: IO ()
main = readFile "input/day14.txt" >>= print . solve
