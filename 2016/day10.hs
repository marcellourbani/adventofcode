#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (find)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)

data Target = BotT Int | Output Int deriving (Show, Eq)

data Instruction
  = Set Int Int
  | Give Int Target Target
  deriving (Show, Eq)

type BotState = M.Map Int (Int, Maybe Int)

type BotRules = M.Map Int (Target, Target)

parse :: String -> [Instruction]
parse s = pl . words <$> lines s
  where
    targ t n = case t of
      "bot" -> BotT (read n)
      _ -> Output (read n)
    pl l = case l of
      "value" : v : _ : _ : _ : t : _ -> Set (read v) (read t)
      _ : bn : _ : _ : _ : ttl : ln : _ : _ : _ : tth : hn : _ -> Give (read bn) (targ ttl ln) (targ tth hn)
      _ -> error "bad"

setBot :: BotState -> Int -> Int -> BotState
setBot botst bot val = case M.lookup bot botst of
  Just (l, _) -> M.insert bot (min l val, Just $max l val) botst
  _ -> M.insert bot (val, Nothing) botst

createRules :: [Instruction] -> BotRules
createRules il = go il M.empty
  where
    go l m = case l of
      [] -> m
      Set _ _ : r -> go r m
      Give b t1 t2 : r -> go r $ M.insert b (t1, t2) m

initialize :: [Instruction] -> BotState
initialize ll = go ll M.empty
  where
    go l m = case l of
      [] -> m
      Set v b : r -> go r $ setBot m b v
      _ : r -> go r m

-- >>> solve $ parse "value 5 goes to bot 2\nbot 2 gives low to bot 1 and high to bot 0\nvalue 3 goes to bot 1\nbot 1 gives low to output 1 and high to bot 0\nbot 0 gives low to output 2 and high to output 0\nvalue 2 goes to bot 2"
-- (0,0)

solve :: [Instruction] -> (Int, Int)
solve is = go initial M.empty 0
  where
    rules = createRules is
    initial = initialize is
    settarg m t v = case t of
      BotT b -> setBot m b v
      Output _ -> m
    setout m t v = case t of
      BotT _ -> m
      Output o -> M.insert o v m
    go botst out g = case completes of
      (bn, (lv, Just hv)) : xs -> case M.lookup bn rules of
        Just (t1, t2) -> go botst' out' g'
          where
            botst' = M.delete bn $ settarg (settarg botst t1 lv) t2 hv
            out' = setout (setout out t1 lv) t2 hv
        _ -> (0, 0)
      _ -> (g, product $ (out M.!) <$> [0 .. 2])
      where
        completes = filter (isJust . snd . snd) $ M.toList botst
        g' = case find ((== (17, Just 61)) . snd) completes of
          Just (b, _) -> b
          _ -> g

main :: IO ()
main = readFile "input/day10.txt" >>= print . solve . parse
