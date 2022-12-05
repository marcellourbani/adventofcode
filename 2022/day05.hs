#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (Foldable (foldl'))
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

type State = (M.Map Int String)

type Move = (Int, Int, Int)

data Input = Input State [Move] deriving (Show)

parse :: String -> Input
parse s = Input columns moves
  where
    [p1, p2] = splitOn [[]] $ lines s
    parsep1 l = [(x, [l !! i]) | (x, i) <- zip [1 ..] [1, 5 .. length l], l !! i /= ' ']
    columns = M.unionsWith (<>) $ map (M.fromAscList . parsep1) <$> take (length p1 -1) $ p1
    parseMove l = (read n, read f, read t) where [_, n, _, f, _, t] = words l
    moves = parseMove <$> p2

applyMove :: State -> Move -> State
applyMove s (n, f, t) = M.update (Just . (moved <>)) t removed
  where
    moved = reverse $ take n $ s M.! f
    removed = M.update (Just . drop n) f s

applyMove2 :: State -> Move -> State
applyMove2 s (n, f, t) = M.update (Just . (moved <>)) t removed
  where
    moved = take n $ s M.! f
    removed = M.update (Just . drop n) f s

-- >>> solve $ parse "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2"
-- ("CMZ","MCD")

solve :: Input -> (String, String)
solve (Input s moves) = (p1, p2)
  where
    p1 = snd <$> M.toList (head <$> foldl' applyMove s moves)
    p2 = snd <$> M.toList (head <$> foldl' applyMove2 s moves)

main :: IO ()
main = readFile "input/day05.txt" >>= print . solve . parse
