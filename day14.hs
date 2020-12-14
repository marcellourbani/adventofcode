#!/usr/bin/env stack
-- stack --resolver lts-10.2 script
module Main where
import           Data.Bits
import Data.List.Split
import qualified Data.Map as M

data Cell = Cell {cellAddr::Int,cellValue::Int} deriving Show
data Mask = Mask {andMask::Int,orMask::Int} deriving Show
data Opcode = OMem Cell| OMask Mask deriving Show

data Machine = Machine {memory::M.Map Int Int,mask::Mask}

parseCode::String -> Opcode
parseCode s = case take 4 s of
  "mem[" -> OMem $ Cell a b where
    [a,b] = read <$> splitOn "] = " (drop 4 s)
  _ -> OMask $ Mask andm orm where
    maxbit = length s - 8
    bits = zip [maxbit,maxbit - 1..0] $ drop 7 s
    orm = sum [2^i| (i,v) <- bits,v == '1' ]
    andm = sum [2^i| (i,v) <- bits,v /= '0' ]

exec :: Machine -> Opcode -> Machine
exec m o = case o of
  OMask ms -> m {mask=ms}
  OMem (Cell a v) -> m {memory = nmem} where
    Mask andm orm = mask m
    nv = andm .&. v .|. orm
    nmem = M.insert a nv  $ memory m

-- >>> solve "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\nmem[8] = 11\nmem[7] = 101\nmem[8] = 0"
-- (165,2)

-- solve :: [Char] -> (Int, Int)
solve s = (first,2)
  where parse = fmap parseCode . lines
        initial = Machine M.empty $ Mask 0 0
        codes = parse s
        firstState = foldl exec initial codes
        first = sum $ M.elems $ memory firstState


main :: IO ()
main = readFile "input/day14.txt" >>= print.solve
