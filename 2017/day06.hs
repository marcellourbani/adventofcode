#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (foldr')
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type State = M.Map Int Int

parse :: String -> State
parse s = M.fromList $ zip [0 ..] $ read <$> words s

next :: State -> State
next s = foldr' (M.updateWithKey upd) s' toUpd
  where
    (i, x) = foldr' cmp (head l) l where l = M.toList s
    cmp a b = if snd b > snd a then b else a
    upd _ x = Just $ x + 1
    s' = M.insert i 0 s
    toUpd = (`mod` M.size s) <$> [i + 1 .. i + x]

-- >>> solve $ parse "0 2 7 0"
-- (5,4)

solve :: State -> (Int, Int)
solve l = go l M.empty
  where
    go s prev = case M.lookup s prev of
      Nothing -> go s' $ M.insert s cur prev
      Just c -> (cur, cur - c)
      where
        s' = next s
        cur = M.size prev

main :: IO ()
main = readFile "input/day06.txt" >>= print . solve . parse
