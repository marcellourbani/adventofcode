#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Bits
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe (isNothing)

data Cell = Cell {cellAddr :: Int, cellValue :: Int} deriving (Show)

data Mask = Mask {andMask :: Int, orMask :: Int, floatingBits :: [Int]} deriving (Show)

data Opcode = OMem Cell | OMask Mask deriving (Show)

data Machine = Machine {memory :: M.Map Int Int, mask :: Mask} deriving (Show)

parseCode :: String -> Opcode
parseCode s = case take 4 s of
  "mem[" -> OMem $ Cell a b
    where
      [a, b] = read <$> splitOn "] = " (drop 4 s)
  _ -> OMask $ Mask andm orm floating
    where
      maxbit = length s - 8
      bits = zip [maxbit, maxbit - 1 .. 0] $ drop 7 s
      orm = sum [2 ^ i | (i, v) <- bits, v == '1']
      andm = sum [2 ^ i | (i, v) <- bits, v /= '0']
      floating = [i | (i, v) <- bits, v == 'X']

exec :: Machine -> Opcode -> Machine
exec m o = case o of
  OMask ms -> m {mask = ms}
  OMem (Cell a v) -> m {memory = nmem}
    where
      Mask andm orm _ = mask m
      nv = andm .&. v .|. orm
      nmem = M.insert a nv $ memory m

clearMap :: [Int] -> Int
clearMap l = sum [2 ^ e | e <- [0 .. 35], isNothing $ find (== e) l]

exec2 :: Machine -> Opcode -> Machine
exec2 m o = case o of
  OMask ms -> m {mask = ms}
  OMem (Cell a v) -> m {memory = nmem}
    where
      Mask _ orm floating = mask m
      clearm = clearMap floating
      bna = clearm .&. a .|. orm
      ofmasks = 0 : (sum <$> expand ((2 ^) <$> floating))
      setmem mem ofs = M.insert (bna .|. ofs) v mem
      nmem = foldl setmem (memory m) ofmasks

remove :: [Int] -> Int -> [Int]
remove l i = take i l ++ drop (i + 1) l

expand :: [Int] -> [[Int]]
expand [] = []
expand l = [[x] | (x, _) <- tmp] ++ [x : y | (x, ys) <- tmp, y <- ys, y /= [] && head y > x]
  where
    tmp = [(l !! i, expand (remove l i)) | i <- [0 .. length l - 1]]

-- >>> solve "mask = 000000000000000000000000000000X1001X\nmem[42] = 100\nmask = 00000000000000000000000000000000X0XX\nmem[26] = 1"
-- (51,207)

-- >>> solve "mask = 000000000000000000000000000000X1001X\nmem[42] = 100"
-- (50,300)

solve :: String -> (Int, Int)
solve s = (first, second)
  where
    parse = fmap parseCode . lines
    initial = Machine M.empty $ Mask 0 0 []
    codes = parse s
    firstState = foldl exec initial codes
    secondState = foldl exec2 initial codes
    first = sum $ M.elems $ memory firstState
    second = sum $ M.elems $ memory secondState

main :: IO ()
main = readFile "input/day14.txt" >>= print . solve
