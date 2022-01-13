#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S

type ExitLink = (M.Map String Int)

parse :: String -> M.Map String ExitLink
parse i = M.unionsWith M.union $ lines i >>= pl
  where
    pl s = case words s of
      [f, _, t, _, d] -> [M.singleton f $ M.singleton t (read d), M.singleton t $ M.singleton f (read d)]
      _ -> error "bad line"

-- >>> solve $ parse "London to Dublin = 464\nLondon to Belfast = 518\nDublin to Belfast = 141"
-- (605,982)

solve :: M.Map String ExitLink -> (Int, Int)
solve m = (p1, p2)
  where
    starts = M.keysSet m
    p1 = maybe 0 fst $ go3 (<) (S.toList starts) S.empty 0 []
    p2 = maybe 0 fst $ go3 (>) (S.toList starts) S.empty 0 []
    pcost f t = m M.! f M.! t

    go3 f nodes seen cost path = case (nodes, length path == S.size starts) of
      (_, True) -> Just (cost, path)
      ([], False) -> Nothing
      (n : ns, _) -> case (child, peer) of
        (Just (cc, _), Just (pc, _)) | f cc pc -> child
        (Just _, Just _) -> peer
        (Nothing, _) -> peer
        _ -> child
        where
          exits = maybe S.empty M.keysSet $ M.lookup n m
          nexts = S.toList $ S.difference exits seen
          seen' = S.insert n seen
          cost' = if null path then cost else cost + pcost (head path) n
          child = go3 f nexts seen' cost' (n : path)
          peer = go3 f ns seen cost path

main :: IO ()
main = readFile "input/day09.txt" >>= print . solve . parse
