#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import Data.Foldable (maximumBy)
import Data.List (find)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Valve = Valve {vId :: String, vFlow :: Int, vDest :: M.Map String Int} deriving (Show, Eq)

type Valves = M.Map String Valve

data State = State {sVid :: String, sTime :: Int, sRate :: Int, sTot :: Int, sVisited :: S.Set String} deriving (Show, Eq)

parse :: String -> Valves
parse s = M.fromList $ zip (vId <$> valves) valves
  where
    valves = pl <$> lines (filter (/= ',') s)
    pl l = Valve vn fr dest
      where
        ws = words l
        vn = ws !! 1
        fr' = drop 5 $ ws !! 4
        fr = read $ take (length fr' - 1) fr'
        dest = M.fromList $ (,1) <$> drop 9 ws

calcDistances :: Valves -> Valves
calcDistances vs
  | added = calcDistances $ M.fromList $ fst <$> nexts
  | otherwise = relevants vs
  where
    isRelevant k (Valve _ f _) = k == "AA" || f > 0
    relevants vm = chdest <$> vm'
      where
        vm' = M.filterWithKey isRelevant vm
        vks = S.delete "AA" $ M.keysSet vm' -- no back path
        chdest v = v {vDest = M.restrictKeys (vDest v) vks}
    nexts = followV vs <$> M.toList vs
    added = or $ snd <$> nexts
    follow wm known (vi, n) = (+ n) <$> M.withoutKeys (vDest (wm M.! vi)) known
    followV vm (vid, v@(Valve _ _ d)) = ((vid, v'), changed)
      where
        known = S.insert vid $ M.keysSet d
        follows = follow vm known <$> M.toList d
        toAdd = M.unionsWith min follows
        changed = M.size toAdd /= 0
        v' = if changed then v {vDest = M.union d toAdd} else v

nextStates :: Int -> Valves -> State -> [State]
nextStates maxt dis s@(State vid t r cur visited)
  | null nexts && t < maxt = [s {sTime = maxt, sTot = cur + r * (maxt - t)}]
  | otherwise = nexts
  where
    nvs = vDest $ dis M.! vid
    nextvs = M.toList $ M.withoutKeys nvs visited'
    visited' = S.insert vid visited
    t' = t + 1
    nexs (i, d) = State i (t' + d) r' (cur + r' + d * r) visited'
      where
        vf = vFlow $ dis M.! i
        r' = r + vf
    nexts = filter ((<= maxt) . sTime) $ nexs <$> nextvs

stateValue :: Int -> State -> Int
stateValue maxt (State _ t r cur _) = (maxt - t) * r + cur

-- >>> solve $ parse "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\nValve BB has flow rate=13; tunnels lead to valves CC, AA\nValve CC has flow rate=2; tunnels lead to valves DD, BB\nValve DD has flow rate=20; tunnels lead to valves CC, AA, EE\nValve EE has flow rate=3; tunnels lead to valves FF, DD\nValve FF has flow rate=0; tunnels lead to valves EE, GG\nValve GG has flow rate=0; tunnels lead to valves FF, HH\nValve HH has flow rate=22; tunnel leads to valve GG\nValve II has flow rate=0; tunnels lead to valves AA, JJ\nValve JJ has flow rate=21; tunnel leads to valve II"

solve :: Valves -> (Int, Int)
solve l = (p1, p2)
  where
    p1 = sTot $ go 30 initial
    p2me = go 26 initial
    p2ele = go 26 $ initial {sVisited = sVisited p2me}
    p2 = sTot p2me + sTot p2ele
    distances = calcDistances l
    initial = State "AA" 1 0 0 S.empty
    cs maxt s1 s2 = compare (stateValue maxt s1) (stateValue maxt s2)
    go maxt s = case nextStates maxt distances s of
      [] -> s
      ns -> maximumBy (cs maxt) $ go maxt <$> ns

main :: IO ()
main = readFile "input/day16.txt" >>= print . solve . parse
