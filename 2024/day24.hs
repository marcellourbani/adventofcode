#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

{-# LANGUAGE PackageImports #-}

module Main where

import Data.Bits
import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import "containers" Data.Map.Strict qualified as M

data OpCode = AND | OR | XOR deriving (Show, Read)

data Operation = Operation {opCode :: OpCode, opOperands :: (String, String), opRes :: String} deriving (Show)

data Device = Device {deState :: M.Map String Int, deOps :: [Operation]} deriving (Show)

parse :: String -> Device
parse s = Device vars $ parseo <$> lines ops
  where
    [vs, ops] = splitOn "\n\n" s
    vars = M.fromList $ parsev <$> lines vs
    parsev a = (take 3 a, read $ drop 4 a)
    parseo a = Operation (read op) (o1, o2) r where [o1, op, o2, _, r] = words a

applyOpCode :: OpCode -> Int -> Int -> Int
applyOpCode oc a b = case oc of
  AND -> a .&. b
  OR -> a .|. b
  XOR -> xor a b

evolve :: Device -> Device
evolve d@(Device st ops) = d {deState = M.union rs st}
  where
    runop (Operation oc (o1, o2) r) = (r,) <$> (applyOpCode oc <$> M.lookup o1 st <*> M.lookup o2 st)
    rs = M.fromList $ mapMaybe runop ops

-- part1 :: Device -> Int
part1 d@(Device st ops) = sum $ uncurry (*) <$> zip (go d) ((2 ^) <$> [0 ..])
  where
    targets = sort $ filter ((== 'z') . head) $ opRes <$> ops
    done dev = all (`M.member` deState dev) targets
    go dev
      | done dev = (deState dev M.!) <$> targets
      | otherwise = go $ evolve dev

-- part2 :: Device -> Int
part2 l = 0

-- >>> solve $ parse "x00: 1\nx01: 1\nx02: 1\ny00: 0\ny01: 1\ny02: 0\n\nx00 AND y00 -> z00\nx01 XOR y01 -> z01\nx02 OR y02 -> z02"
-- >>> solve $ parse "x00: 1\nx01: 0\nx02: 1\nx03: 1\nx04: 0\ny00: 1\ny01: 1\ny02: 1\ny03: 1\ny04: 1\n\nntg XOR fgs -> mjb\ny02 OR x01 -> tnw\nkwq OR kpj -> z05\nx00 OR x03 -> fst\ntgd XOR rvg -> z01\nvdt OR tnw -> bfw\nbfw AND frj -> z10\nffh OR nrd -> bqk\ny00 AND y03 -> djm\ny03 OR y00 -> psh\nbqk OR frj -> z08\ntnw OR fst -> frj\ngnj AND tgd -> z11\nbfw XOR mjb -> z00\nx03 OR x00 -> vdt\ngnj AND wpb -> z02\nx04 AND y00 -> kjc\ndjm OR pbm -> qhw\nnrd AND vdt -> hwm\nkjc AND fst -> rvg\ny04 OR y02 -> fgs\ny01 AND x02 -> pbm\nntg OR kjc -> kwq\npsh XOR fgs -> tgd\nqhw XOR tgd -> z09\npbm OR djm -> kpj\nx03 XOR y03 -> ffh\nx00 XOR y04 -> ntg\nbfw OR bqk -> z06\nnrd XOR fgs -> wpb\nfrj XOR qhw -> z04\nbqk OR frj -> z07\ny03 OR x01 -> nrd\nhwm AND bqk -> z03\ntgd XOR rvg -> z12\ntnw OR pbm -> gnj"
-- (4,0)
-- (2024,0)

solve :: Device -> (Int, Int)
solve l = (p1, p2)
  where
    p1 = part1 l
    p2 = part2 l

main :: IO ()
main = readFile "input/day24.txt" >>= print . solve . parse
