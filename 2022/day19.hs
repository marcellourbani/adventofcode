#!/usr/bin/env stack
-- stack --resolver lts-18.18 script --optimize

{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

-- dfs and prune poorly translated from https://colab.research.google.com/github/norvig/pytudes/blob/main/ipynb/Advent-2022.ipynb

import Data.Char (toUpper)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

data Material = Ore | Clay | Obsidian | Geode deriving (Show, Read, Eq, Ord, Enum)

type MaterialMap = M.Map Material Int

data State = State {sRobs :: !MaterialMap, sMats :: !MaterialMap, sTime :: !Int} deriving (Show, Eq, Ord)

type Blueprint = M.Map Material MaterialMap

type Limits = MaterialMap

type StateKey = (Int, Int, Int, Int, Int)

type Visited = M.Map StateKey MaterialMap

parse :: String -> [(Int, Blueprint)]
parse s = parseLine <$> lines s
  where
    parseLine l = (read n, robs)
      where
        [n, rest] = splitOn ":" (drop 10 l)
        rawrobs = tail $ splitOn " Each " $ filter (/= '.') rest
        robs = M.fromList $ parseBp <$> rawrobs
        parseBp r = (parseM rm, M.fromList $ parseC <$> splitOn ["and"] rms) where rm : _ : _ : rms = words r
        parseM m = read $ toUpper (head m) : tail m
        parseC [cn, cm] = (parseM cm, read cn)
        parseC _ = undefined

(??) :: MaterialMap -> Material -> Int
(??) mm m = fromMaybe 0 $ mm M.!? m

srob :: State -> Material -> Int
srob s m = sRobs s ?? m

smat :: State -> Material -> Int
smat s m = sMats s ?? m

stateKv :: State -> (StateKey, MaterialMap)
stateKv (State r m t) = ((r ?? Ore, r ?? Clay, r ?? Obsidian, r ?? Geode, t), m)

getLimits :: Blueprint -> Limits
getLimits = M.foldr' (M.unionWith max) M.empty

collect :: State -> State
collect (State robs mats timeleft) = State robs (M.unionWith (+) robs mats) $ timeleft - 1

contained :: MaterialMap -> MaterialMap -> Bool
contained existing required = all (\(comp, q) -> q <= existing ?? comp) $ M.toList required

nexts :: Blueprint -> Limits -> State -> [State]
nexts bprint limits s
  | sTime s <= 0 = []
  | otherwise = base : (buildRobot <$> toBuild)
  where
    base@(State robs mats tl) = collect s
    buildRobot m = State (M.unionWith (+) robs $ M.singleton m 1) (M.unionWith (-) mats $ bprint M.! m) tl
    shouldbuild m = m == Geode || limits ?? m > robs ?? m
    canbuild m = contained (sMats s) $ bprint M.! m
    toBuild = [r | r <- M.keys bprint, shouldbuild r, canbuild r]

pruneState :: State -> Visited -> State -> (Bool, Visited)
pruneState candidate visited best = case cached of
  Just old | contained old cur -> (True, visited)
  _ -> (upperbound <= smat best Geode, M.insert key cur visited)
  where
    (key, cur) = stateKv candidate
    cached = visited M.!? key
    georobs = srob candidate Geode
    upperbound = smat candidate Geode + sum [georobs .. georobs + sTime candidate]

maxGeodes :: State -> Blueprint -> State
maxGeodes initial bp = go [initial] M.empty initial
  where
    limits = getLimits bp
    go states visited best = case states of
      [] -> best
      s : ss
        | smat s Geode > smat best Geode -> go states' visited' s
        | otherwise -> go states' visited' best
        where
          best' = if smat s Geode > smat best Geode then s else best
          (pruned, visited') = pruneState s visited best
          states' = if sTime s == 0 || pruned then ss else nexts bp limits s <> ss

qualityLevel :: State -> (Int, Blueprint) -> Int
qualityLevel s (bi, bp) = bi * smat (maxGeodes s bp) Geode

example :: [(Int, Blueprint)]
example = parse "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.\nBlueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."

-- >>> solve example

solve :: [(Int, Blueprint)] -> (Int, Int)
solve l = (p1, p2)
  where
    initial = State (M.singleton Ore 1) M.empty 24
    initial2 = State (M.singleton Ore 1) M.empty 32
    p1 = sum $ qualityLevel initial <$> l
    p2 = product $ (?? Geode) . sMats . maxGeodes initial2 . snd <$> take 3 l

main :: IO ()
main = readFile "input/day19.txt" >>= print . solve . parse
