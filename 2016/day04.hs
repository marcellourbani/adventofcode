#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List (find, isPrefixOf, sortBy)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

data Room = Room {secid :: Int, csum :: String, parts :: [String]} deriving (Show)

parse :: String -> [Room]
parse s = pl <$> lines s
  where
    pl l = case splitOn "[" l of
      [a, b] -> Room sid csum $ take (length parts -1) parts
        where
          parts = splitOn "-" a
          csum = take (length b - 1) b
          sid = read $last parts
      _ -> error $ "bad line: " ++ l

isReal :: Room -> Bool
isReal (Room _ csum ps) = csum `isPrefixOf` mostf -- True
  where
    freqs = M.toList $ M.unionsWith (+) $ M.singleton <$> concat ps <*> [1]
    c (c1, n1) (c2, n2) = compare (n2, c1) (n1, c2)
    mostf = fst <$> sortBy c freqs

decrypt :: Room -> String
decrypt (Room sid _ ct) = unwords $fmap (decryptc sid) <$> ct
  where
    decryptc i c = toEnum $ mi + mod cc bas
      where
        mi = fromEnum 'a'
        bas = 1 + fromEnum 'z' - mi
        cc = fromEnum c - mi + i

-- >>> solve $ parse "aaaaa-bbb-z-y-x-123[abxyz]\na-b-c-d-e-f-g-h-987[abcde]\nnot-a-real-room-404[oarel]\ntotally-real-room-200[decoy]"
-- (1514,0)

solve :: [Room] -> (Int, Int)
solve l = (p1, p2)
  where
    validRooms = filter isReal l
    p1 = sum $ secid <$> validRooms
    p2 = maybe 0 secid $ find ((== "northpole object storage") . decrypt) validRooms

main :: IO ()
main = readFile "input/day04.txt" >>= print . solve . parse
