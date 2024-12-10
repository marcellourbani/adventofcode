#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

{-# LANGUAGE PackageImports #-}

module Main where

import Data.List (filter, sortBy)
import Data.Ord (Ord (compare))
import "containers" Data.Map.Strict qualified as M

data FileChunk = FileChunk {fcId :: Int, fcStart :: Int, fcLen :: Int} deriving (Show)

type Input = [FileChunk]

parse :: String -> Input
parse s = go 0 0 s'
  where
    s' = if odd $ length s then s <> "0" else s
    fc fid so l e xs = FileChunk fid so le : go (fid + 1) (so + le + sp) xs where le = read $ l : ""; sp = read $ e : ""
    go fid so st = case st of
      l : e : xs -> FileChunk fid so le : go (fid + 1) (so + le + sp) xs where le = read $ l : ""; sp = read $ e : ""
      _ -> []

part1 :: Input -> Int
part1 i = sum $ uncurry (*) <$> zip [0 ..] ids
  where
    maxid = length i
    ids = go toAssign [] 0 maxid
    toAssign = M.fromAscList $ zip (fcId <$> i) i
    consume m i n = case M.lookup i m of
      Just f@(FileChunk _ _ le)
        | le > n -> Just (i, n, M.insert i (f {fcLen = le - n}) m)
        | otherwise -> Just (i - 1, le, M.delete i m)
      Nothing -> Nothing

    go chunks acc cur las = case M.lookup cur chunks of
      Nothing
        | cur >= las || M.null chunks -> acc
        | otherwise -> go chunks acc (cur + 1) las
      Just (FileChunk _ fofs flen)
        | length acc == fofs -> go (M.delete cur chunks) (acc <> replicate flen cur) (cur + 1) las
        | otherwise -> case consume chunks las (fofs - length acc) of
            Nothing -> go chunks acc cur $ las - 1
            Just (las', cn, chunks') -> go chunks' (acc <> replicate cn las) cur las'

part2 :: Input -> Int
part2 i = sum $ score <$> chunks
  where
    chunks = go spaces M.empty $ reverse i
    score (FileChunk ci cst cle) = sum $ uncurry (*) <$> zip (replicate cle ci) [cst ..]
    spaces = sp i
    sp l = case l of
      (FileChunk _ fofs flen) : x@(FileChunk _ fofs2 _) : xs
        | fofs2 > fofs + flen -> (fofs + flen, fofs2 - fofs - flen) : sp (x : xs)
        | otherwise -> sp (x : xs)
      _ -> []
    fit pre spl fc@(FileChunk _ st le) = case spl of
      [] -> Nothing
      s@(ofs, slen) : os
        | ofs > st -> Nothing
        | slen >= le -> Just (fc {fcStart = ofs}, reverse pre <> [(ofs + le, slen - le)] <> os)
        | otherwise -> fit (s : pre) os fc
    byOfs a b = compare (fcStart a) (fcStart b)
    go spl acc l = case l of
      [] -> sortBy byOfs $ snd <$> M.toList acc
      x : xs -> case fit [] spl x of
        Nothing -> go spl (M.insert (fcId x) x acc) xs
        Just (x', spl') -> go spl' (M.insert (fcId x') x' acc) xs

-- >>> solve $ parse "2333133121414131402"
-- (1928,2858)

solve :: Input -> (Int, Int)
solve l = (p1, p2)
  where
    p1 = part1 l
    p2 = part2 l

main :: IO ()
main = readFile "input/day09.txt" >>= print . solve . parse
