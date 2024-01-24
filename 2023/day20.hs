#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import Control.Arrow (second)
import Data.Foldable (foldl')
import Data.List (partition)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Debug.Trace (trace)

type SignalLevel = Bool

data Signal = Signal {sFrom :: String, sTo :: String, sLevel :: SignalLevel} deriving (Show, Eq, Ord)

data Module
  = FlipFlop {fmName :: String}
  | Conjunction {cmName :: String, cmInputs :: [String]}
  | Simple {smName :: String}
  deriving (Show, Eq, Ord)

data Machine = Machine {wModules :: M.Map String Module, wWires :: M.Map String [String]} deriving (Show)

type Memory = M.Map String SignalLevel

memoryAt :: Memory -> String -> SignalLevel
memoryAt mem k = M.findWithDefault False k mem

parse :: String -> Machine
parse s = Machine modules wires
  where
    entries = splitOn " -> " <$> lines (filter (/= ',') s)
    memory = parseModule <$> entries
    targets [k, t] = M.singleton (filter (`notElem` "%&") k) $ words t
    wires = M.unions $ targets <$> entries
    sources k = M.keys $ M.filter (elem k) wires
    modules = M.fromList $ parseModule <$> entries
    parseModule [k, t] = case k of
      "broadcaster" -> (k, Conjunction k [])
      '%' : n -> (n, FlipFlop n)
      '&' : n -> (n, Conjunction n $ sources n)

button :: Signal
button = Signal "button" "broadcaster" False

processSignal :: Machine -> Memory -> Signal -> (Memory, [Signal])
processSignal (Machine mods wires) mem (Signal src dest lev) = case M.lookup dest mods of
  Nothing -> (mem, [])
  Just (Simple _) -> (mem, [])
  Just (FlipFlop _) | lev -> (mem, [])
  Just (FlipFlop _) -> (setMem $ not current, toSend $ not current)
  Just (Conjunction _ inputs) -> (setMem v, toSend v) where v = not $ all (memoryAt mem) inputs
  where
    targets = M.findWithDefault [] dest wires
    toSend v = Signal dest <$> targets <*> [v]
    setMem v = M.insert dest v mem
    current = M.findWithDefault False dest mem

sendPulse :: Machine -> Memory -> (Memory, M.Map Bool Int)
sendPulse machine mem = go mem [button] M.empty
  where
    go mem sigs count = case sigs of
      [] -> (mem, count)
      s : ss -> go mem' sigs' count'
        where
          (mem', secs) = processSignal machine mem s
          count' = M.unionWith (+) count $ M.singleton (sLevel s) 1
          sigs' = ss <> secs

countSignals :: Machine -> Memory -> Int -> M.Map Bool Int
countSignals machine memory = go memory (M.singleton False 0)
  where
    go mem count n = case n of
      0 -> count
      _ -> go mem' count'' (n - 1)
        where
          (mem', count') = sendPulse machine mem
          count'' = M.unionWith (+) count count'

calcreceive :: Machine -> String -> Int
calcreceive machine@(Machine nodes wires) target = findFreq M.empty 0 (findLayer target) [] []
  where
    reversed = M.unionsWith (<>) [M.singleton v [k] | (k, vs) <- M.toList wires, v <- vs]
    -- assumes target is preceded by a single conjunction preceded by many conjunctions
    findLayer t = case (M.lookup t nodes, M.lookup t reversed) of
      (_, Just [i]) -> findLayer i
      (Just (Conjunction _ is), _) -> is
      _ -> []

    tomult = findLayer target
    findFreq mem n targets counts sigs = case (targets, sigs) of
      ([], _) -> product counts
      (_, []) -> findFreq mem (n + 1) targets counts [button]
      (_, s : ss) -> findFreq mem' n targets' counts' sigs'
        where
          (mem', secs) = processSignal machine mem s
          (reached, targets') = partition (memoryAt mem') targets
          counts' = counts <> (n <$ reached)
          sigs' = ss <> secs

-- >>> solve $ parse "broadcaster -> a\n%a -> inv, con\n&inv -> b\n%b -> con\n&con -> output"
-- (11687500,1)

solve :: Machine -> (Int, Int)
solve m = (p1, p2)
  where
    p1 = product $ countSignals m M.empty 1000
    p2 = calcreceive m "rx"

main :: IO ()
main = readFile "input/day20.txt" >>= print . solve . parse
