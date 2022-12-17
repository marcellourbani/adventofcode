#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (maximumBy)
import Data.List (find)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Valve = Valve {vId :: String, vFlow :: Int, vDest :: M.Map String Int} deriving (Show, Eq)

type Valves = M.Map String Valve

data State = State {sVid :: String, sTime :: Int, sRate :: Int, sTot :: Int, sVisited :: S.Set String} deriving (Show, Eq)

data State2 = State2 {s2Me :: (String, Int), s2Elep :: (String, Int), s2Time :: Int, s2Rate :: Int, s2Tot :: Int, s2Visited :: [String]} deriving (Show, Eq)

parse :: String -> Valves
parse s = M.fromList $ zip (vId <$> valves) valves
  where
    valves = pl <$> lines (filter (/= ',') s)
    pl l = Valve vn fr dest
      where
        ws = words l
        vn = ws !! 1
        fr' = drop 5 $ ws !! 4
        fr = read $ take (length fr' -1) fr'
        dest = M.fromList $ zip (drop 9 ws) (repeat 1)

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

nextStates :: Valves -> State -> [State]
nextStates dis s@(State vid t r cur visited)
  | null nexts && t < 30 = [s {sTime = 30, sTot = cur + r * (30 - t)}]
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
    nexts = filter ((<= 30) . sTime) $ nexs <$> nextvs

stateValue :: State -> Int
stateValue (State _ t r cur _) = (30 - t) * r + cur

state2Value :: State2 -> Int
state2Value (State2 _ _ t r cur _) = (26 - t) * r + cur

nextvalves :: Valves -> [String] -> (String, Int) -> [(String, Int)]
nextvalves dis visited (val, vald)
  | vald > 0 = []
  | otherwise = M.toList $ M.withoutKeys dests $ S.fromList visited
  where
    dests = vDest $ dis M.! val

advanceS2 :: Valves -> State2 -> State2
advanceS2 dis s@(State2 me@(myv, myt) ele@(elv, elt) t r cur visited) = s'
  where
    t' = t + 1
    cur' = cur + r
    dec x
      | x == 0 = 0
      | otherwise = x -1
    me' = (myv, dec myt)
    ele' = (elv, dec elt)

    toAdd (vid, vn) = (vn == 0) && notElem vid visited
    added = S.toList $ S.fromList $ fst <$> filter toAdd [me', ele']
    r' = r + sum (vFlow . (dis M.!) <$> added)
    visited' = visited <> added
    s' = State2 me' ele' t' r' cur' visited'

nextStates2 :: Valves -> State2 -> [State2]
nextStates2 dis s@(State2 me@(myv, myt) ele@(elv, elt) t r cur visited)
  | t >= 26 = []
  | otherwise = nexts
  where
    visited' = myv : elv : visited
    nv = nextvalves dis visited'
    s' = advanceS2 dis s
    swapped = case (nv me, nv ele) of
      ([], []) -> []
      ([], eles) -> [s' {s2Elep = e} | e <- eles]
      (mes, []) -> [s' {s2Me = m} | m <- mes]
      (mes, eles) -> [s' {s2Me = m, s2Elep = e} | e <- eles, m <- mes, fst m /= fst e]
    nexts = case swapped of
      [] -> []
      _ -> toRelevant dis <$> swapped

toRelevant :: Valves -> State2 -> State2
toRelevant dis st@(State2 me' ele' t' _ _ vis')
  | hasDest vis' me' && hasDest vis' ele' && t' < 26 = toRelevant dis $ advanceS2 dis st
  | otherwise = st
  where
    hasDest openv (vid, vt) = vt > 0 && notElem vid openv

-- >>> solve $ parse "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\nValve BB has flow rate=13; tunnels lead to valves CC, AA\nValve CC has flow rate=2; tunnels lead to valves DD, BB\nValve DD has flow rate=20; tunnels lead to valves CC, AA, EE\nValve EE has flow rate=3; tunnels lead to valves FF, DD\nValve FF has flow rate=0; tunnels lead to valves EE, GG\nValve GG has flow rate=0; tunnels lead to valves FF, HH\nValve HH has flow rate=22; tunnel leads to valve GG\nValve II has flow rate=0; tunnels lead to valves AA, JJ\nValve JJ has flow rate=21; tunnel leads to valve II"
-- (1651,1707)

solve :: Valves -> (Int, Int)
solve l = (p1, p2)
  where
    p1 = sTot $ go initial
    p2 = s2Tot $ go2 initial2
    distances = calcDistances l
    initial = State "AA" 1 0 0 S.empty
    initial2 = State2 ("AA", 0) ("AA", 0) 0 0 0 []
    cs s1 s2 = compare (stateValue s1) (stateValue s2)
    go s = case nextStates distances s of
      [] -> s
      ns -> maximumBy cs $ go <$> ns

    cs2 s1 s2 = compare (state2Value s1) (state2Value s2)
    to26 s
      | s2Time s >= 26 = s
      | otherwise = to26 $ advanceS2 distances s
    go2 s = case nextStates2 distances s of
      [] -> to26 s
      ns -> maximumBy cs2 $ go2 <$> ns

main :: IO ()
main = readFile "input/day16.txt" >>= print . solve . parse