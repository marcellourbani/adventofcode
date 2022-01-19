#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Control.Arrow (first, second)
import Data.Foldable (foldl')
import Data.Function (on)
import Data.List (find, sortBy)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, isNothing)
import qualified Data.PQueue.Prio.Min as P
import qualified Data.Set as S

data Player = Player {_phealth :: Int, _pmana :: Int} deriving (Show, Eq, Ord)

data Boss = Boss {_bhealth :: Int, _bDamage :: Int} deriving (Show, Eq, Ord)

data Action = MagicMissile | Drain deriving (Show, Eq)

data Effect = Shield | Poison | Recharge deriving (Show, Eq, Ord)

data Spell = Immediate Action | Delayed Effect deriving (Show, Eq)

data Outcome = Win | Loss deriving (Show, Eq)

data State = State {_b :: Boss, _p :: Player, _e :: [(Effect, Int)]} deriving (Show, Eq, Ord)

parse :: String -> Boss
parse i = Boss p d
  where
    [p, d] = read . last . words <$> lines i

duration :: Effect -> Int
duration e
  | e == Recharge = 5
  | otherwise = 6

possibleSpells :: [Effect] -> Int -> [Spell]
possibleSpells e mp = filter ((<= mp) . cost) spells
  where
    spells = (Immediate <$> [MagicMissile, Drain]) <> (Delayed <$> filter (`notElem` e) [Shield, Poison, Recharge])

cost :: Spell -> Int
cost s = case s of
  Immediate MagicMissile -> 53
  Immediate Drain -> 73
  Delayed Shield -> 113
  Delayed Poison -> 173
  Delayed Recharge -> 229

applyEffect :: State -> Effect -> State
applyEffect s@(State b@(Boss bh bd) p@(Player ph pm) e) eff = case eff of
  Poison -> State (Boss (bh -3) bd) p e
  Recharge -> State b (Player ph (pm + 101)) e
  _ -> s

applySpell :: State -> Spell -> (Int, State)
applySpell s@(State b@(Boss bh bd) p@(Player ph pm) e) sp = (cost sp, s')
  where
    p' = Player ph $ pm - cost sp
    s' = case sp of
      Immediate MagicMissile -> State (Boss (bh -4) bd) p' e
      Immediate Drain -> State (Boss (bh -2) bd) p' {_phealth = ph + 2} e
      Delayed eff -> State b p' ((eff, duration eff) : e)

-- generic djikstra-ish search
findState :: Ord a => (a -> Bool) -> (a -> [(Int, (a, [t]))]) -> a -> (a, Int, [t])
findState exitCond neighbours start = go S.empty (P.singleton 0 (start, []))
  where
    go visited costs
      | exitCond curst = (curst, cost, curtr)
      | S.member curst visited = go visited' costs'
      | otherwise = go visited' costs''
      where
        (cost, (curst, curtr)) = P.findMin costs
        visited' = S.insert curst visited
        costs' = P.deleteMin costs
        newNodes = first (+ cost) <$> filter ((`S.notMember` visited) . fst . snd) (neighbours curst)
        addCost cs (c, (st, tr)) = P.insert c (st, curtr <> tr) cs
        costs'' = foldl' addCost costs' newNodes

nextStates :: State -> [(Int, (State, [Spell]))]
nextStates s@(State _ (Player _ mp) _) = filter ((/= Just Loss) . outcome . fst . snd) states
  where
    states = turn s <$> possibleSpells effs mp
    effs = fst <$> filter ((/= 1) . snd) (_e s)

nextStates2 :: State -> [(Int, (State, [Spell]))]
nextStates2 s@(State _ (Player _ mp) _) = filter ((/= Just Loss) . outcome . fst . snd) states
  where
    states = turn2 s <$> possibleSpells effs mp
    effs = fst <$> filter ((/= 1) . snd) (_e s)

outcome :: State -> Maybe Outcome
outcome (State b p _) = case (b, p) of
  (Boss p _, _) | p <= 0 -> Just Win
  (_, Player p _) | p <= 0 -> Just Loss
  _ -> Nothing

findWinner :: State -> (State, Int, [Spell])
findWinner = findState ((== Just Win) . outcome) nextStates

findWinner2 :: State -> (State, Int, [Spell])
findWinner2 = findState ((== Just Win) . outcome) nextStates2

turn :: State -> Spell -> (Int, (State, [Spell]))
turn s@(State _ _ effs) sp
  | _bhealth (_b s') <= 0 = (0, (s', []))
  | otherwise = (c'', (bossTurn s'', [sp]))
  where
    ageEff (e, a) = (e, a - 1)
    eff' = filter ((> 0) . snd) $ map ageEff effs
    s' = (foldl' applyEffect s $fst <$> effs) {_e = eff'}
    (c'', s'') = applySpell s' sp
    bossTurn st@(State (Boss _ bhits) p@(Player ph pm) e')
      | _bhealth (_b st') <= 0 = st'
      | otherwise = st' {_p = Player (ph - bossHits) pm'}
      where
        st'@(State _ (Player _ pm') _) = (foldl' applyEffect st $fst <$> e') {_e = eff''}
        eff'' = filter ((> 0) . snd) $ map ageEff e'
        shield = if isNothing (find ((== Shield) . fst) e') then 0 else 7
        bossHits = max 1 $ bhits - shield

turn2 :: State -> Spell -> (Int, (State, [Spell]))
turn2 s@(State _ (Player ph pm) _) sp
  | ph <= 1 = (0, (s', []))
  | otherwise = turn s' sp
  where
    p' = Player (ph -1) pm
    s' = s {_p = p'}

-- >>> solve $ parse"Hit Points: 55\nDamage: 8"
-- (953,1289,[Delayed Poison,Delayed Recharge,Delayed Shield,Delayed Poison,Delayed Recharge,Immediate Drain,Delayed Poison,Immediate Drain,Immediate MagicMissile])

solve :: Boss -> (Int, Int)
solve enemy = (p1, p2)
  where
    initial = State enemy (Player 50 500) []
    (_, p1, _) = findWinner initial
    (_, p2, s2) = findWinner2 initial

main :: IO ()
main = readFile "input/day22.txt" >>= print . solve . parse
