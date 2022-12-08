#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List (sort)

data Cmd = Ls | Cd String deriving (Show)

data File = File String Int | Dir String deriving (Show)

type Input = Either File Cmd

data Tree = Node
  { dir :: String,
    children :: [Tree],
    files :: [(String, Int)]
  }
  deriving (Show)

parse :: String -> [Input]
parse s = parseline <$> lines s
  where
    parseline l = case words l of
      ["$", "ls"] -> Right Ls
      ["$", "cd", d] -> Right $ Cd d
      ["dir", d] -> Left $ Dir d
      [s, f] -> Left $ File f $ read s
      _ -> undefined

buildTree :: [Input] -> Tree
buildTree input = go input root []
  where
    root = Node "/" [] []
    go inp n@(Node dn ch fi) par = case inp of
      []
        | null par -> n
        | otherwise -> go [Right (Cd "..")] n par
      (Left (File fn fs)) : is -> go is (n {files = (fn, fs) : fi}) par
      (Left (Dir dn)) : is -> go is n par
      (Right Ls) : is -> go is n par
      (Right (Cd "..")) : is -> case par of
        (p : ps) -> go is (p {children = n : children p}) ps
        _ -> undefined
      (Right (Cd "/")) : is
        | dn == "/" -> go is n par
        | otherwise -> go inp (p {children = n : children p}) (tail par)
        where
          p = head par
      (Right (Cd d)) : is -> go is (Node d [] []) $ n : par

dirSizes :: Tree -> [(String, Int)]
dirSizes r@(Node dn ch _) = (dn, siz r) : (ch >>= dirSizes)
  where
    siz r@(Node _ ch fi) = sum (siz <$> ch) + sum (snd <$> fi)

-- >>> solve $ parse "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k"
-- (95437,24933642)

solve :: [Input] -> (Int, Int)
solve l = (p1, p2)
  where
    t = buildTree l
    dss = dirSizes t
    p1 = sum $ filter (< 100000) $ snd <$> dss
    p2 = minimum (filter (> target) $snd <$> dss)
    target = snd (head dss) - 40000000

main :: IO ()
main = readFile "input/day07.txt" >>= print . solve . parse
