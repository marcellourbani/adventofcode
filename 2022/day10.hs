#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Instruction = Noop | Addx Int deriving (Show, Eq)

data CPU = CPU {cPC :: Int, cX :: Int, cStrengths :: [Int]} deriving (Show)

newtype Crt = Crt (S.Set (Int, Int))

crtMaxLine = 5

crtMaxCol = 39

instance Show Crt where
  show (Crt s) = "\n" <> unlines (sl <$> [0 .. crtMaxLine])
    where
      sl i = [if S.member (x, i) s then '#' else '.' | x <- [0 .. crtMaxCol]]

parse :: String -> [Instruction]
parse s = pl <$> lines s
  where
    pl l = case words l of
      ["addx", n] -> Addx $ read n
      _ -> Noop

exec :: CPU -> Instruction -> CPU
exec cpu@(CPU pc x str) ins = case ins of
  Noop -> CPU pc' x str'
  Addx n -> CPU pc' (x + n) str'
  where
    (pc', str') = go pc str $ if ins == Noop then 1 else 2
    go p s n
      | n == 0 = (p, s)
      | otherwise = go (p + 1) s' (n - 1)
      where
        s' = if p >= 20 && 0 == mod (p - 20) 40 then p * x : s else s

xpos :: (CPU, M.Map Int Int) -> Instruction -> (CPU, M.Map Int Int)
xpos (cpu@(CPU pc x _), m) ins = (cpu', m')
  where
    cpu'@(CPU pc' x' _) = exec cpu ins
    m' = M.union m $ M.fromList $ (pc', x') : zip [pc .. pc' -1] (repeat x)

draw :: M.Map Int Int -> Crt
draw m = Crt $ S.fromList $ filter pixel2 coords
  where
    coords = [(x, y) | x <- [0 .. crtMaxCol], y <- [0 .. crtMaxLine]]
    finalClock = (crtMaxCol + 1) * (crtMaxLine + 1)
    clock (x, y) = 1 + x + ((crtMaxCol + 1) * y)
    pixel2 c@(x, y) = case M.lookup (clock c) m of
      Nothing -> False
      Just x1 -> x1 >= (x -1) && x1 <= (x + 1)

-- >>> solve $ parse "noop\naddx 3\naddx -5"
-- >>> solve $ parse "addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4\nnoop\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx -35\naddx 1\naddx 24\naddx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\nnoop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8\naddx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\nnoop\naddx -36\nnoop\naddx 1\naddx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop\nnoop\nnoop\nnoop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\naddx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop\naddx 8\nnoop\naddx -1\naddx 2\naddx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\nnoop\naddx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\naddx 26\naddx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9\naddx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1\naddx 2\naddx -37\naddx 1\naddx 3\nnoop\naddx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2\naddx 1\nnoop\naddx -10\nnoop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\naddx -6\naddx -11\nnoop\nnoop\nnoop\n"
-- (0,
-- #####...................................
-- ........................................
-- ........................................
-- ........................................
-- ........................................
-- ........................................
-- )
-- (13140,
-- ##..##..##..##..##..##..##..##..##..##..
-- ###...###...###...###...###...###...###.
-- ####....####....####....####....####....
-- #####.....#####.....#####.....#####.....
-- ######......######......######......####
-- #######.......#######.......#######.....
-- )

solve :: [Instruction] -> (Int, Crt)
solve l = (p1, p2)
  where
    ll = parse "noop\naddx 3\naddx -5\nnoop\nnoop" -- take 20 l
    initial = CPU 1 1 []
    (final, visited) = foldl' xpos (initial, M.empty) l
    p1 = sum $ cStrengths final
    p2 = draw visited

main :: IO ()
main = readFile "input/day10.txt" >>= print . solve . parse
