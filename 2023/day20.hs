#!/usr/bin/env stack
-- stack --resolver lts-18.18 script --optimize

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import Control.Arrow (second)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data SignalLevel = High | Low deriving (Show, Eq, Ord, Enum)

data Signal = Signal {sFrom :: String, sTo :: String, sLevel :: SignalLevel} deriving (Show, Eq, Ord)

data Module
  = Broadcaster {bmTargets :: [String]}
  | FlipFlop {fmName :: String, fmTargets :: [String], fmState :: SignalLevel}
  | Conjunction {cmName :: String, cmTargets :: [String], cmInputs :: M.Map String SignalLevel}
  | Untyped {umName :: String, umState :: SignalLevel}
  deriving (Show, Eq, Ord)

data State = State {stMemory :: M.Map String Module, stHighs :: Int, stLows :: Int} deriving (Show, Eq, Ord)

mTargets :: Module -> [String]
mTargets m = case m of
  Broadcaster ts -> ts
  FlipFlop _ ts _ -> ts
  Conjunction _ ts _ -> ts
  _ -> []

parse :: String -> State
parse s = State (M.fromList $ second addSources <$> rawModules) 0 0
  where
    entries = splitOn " -> " <$> lines (filter (/= ',') s)
    rawModules = parseModule <$> entries
    sources n = fst <$> filter ((n `elem`) . mTargets . snd) rawModules
    addSources m = case m of
      Conjunction n ts _ -> m {cmInputs = M.fromList $ (,Low) <$> sources n}
      _ -> m
    parseModule [k, t] = case k of
      "broadcaster" -> ("broadcaster", Broadcaster $ words t)
      '%' : n -> (n, FlipFlop n (words t) Low)
      '&' : n -> (n, Conjunction n (words t) M.empty)

readState :: State -> String -> Module
readState s k = M.findWithDefault (Untyped k High) k $ stMemory s

processSignal :: Signal -> State -> State
processSignal signal@(Signal _ _ lev) state = go (incS lev [1] state) [signal] []
  where
    invert st = if st == High then Low else High
    incS lev l (State mem hi lo)
      | lev == High = State mem (hi + length l) lo
      | otherwise = State mem hi (lo + length l)
    go m s nxs = case (s, nxs) of
      ([], []) -> m
      (s1 : ss, _) -> go m' ss $ nxs <> nx' where (m', nx') = stepP m s1
      ([], _) -> go m nxs []
    stepP st@(State mem hi lo) s@(Signal from to lev) = case (readState st to, lev) of
      (Broadcaster l, _) -> (incS lev l st, Signal "broadcaster" <$> l <*> [lev])
      (FlipFlop {}, High) -> (st, [])
      (FlipFlop n l st, Low) -> (incS lev' l $ State mem' hi lo, Signal to <$> l <*> [invert st])
        where
          lev' = invert st
          mem' = M.insert n (FlipFlop n l lev') mem
      (Untyped n High, Low) -> (State (M.insert n (Untyped n lev) mem) hi lo, [])
      (Untyped _ _, _) -> (st, [])
      (Conjunction n l inp, _) -> (incS lev' l $ State mem' hi lo, Signal to <$> l <*> [lev'])
        where
          inp' = M.insert from lev inp
          lev' = if all (== High) inp' then Low else High
          mem' = M.insert n (Conjunction n l inp') mem

-- >>> solve "output" $ parse "broadcaster -> a\n%a -> inv, con\n&inv -> b\n%b -> con\n&con -> output"
-- (11687500,1)
solve out l = (p1, p2 l)
  where
    button = Signal "button" "broadcaster" Low
    p1final = iterate (processSignal button) l !! 1000
    state m = case m of
      FlipFlop _ _ st -> Just st
      Untyped _ st -> Just st
      _ -> Nothing
    states s = M.toList $ M.mapMaybe state (stMemory s)
    p1 = stHighs p1final * stLows p1final
    -- brute force will work eventually,might take ages
    p2 st
      | umState (readState st out) == Low = 0
      | otherwise = 1 + p2 (processSignal button st)

main :: IO ()
main = readFile "input/day20.txt" >>= print . solve "rx" . parse
