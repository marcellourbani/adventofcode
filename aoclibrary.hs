-- couldn't find a way to import this as a module in stack scripts, will just copy and paste
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module AocLibrary (GameMap, mapTile) where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.PQueue.Prio.Min as P

data GameMap c = GameMap {gmW :: Int, gmH :: Int, gmMap :: M.Map (Int, Int) c} deriving (Eq)

instance Functor GameMap where
  fmap f (GameMap w h m) = GameMap w h $ M.map f m

instance Show (GameMap Char) where
  show gm@(GameMap w h m) = unlines [[mapTile '.' gm (x, y) | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]

instance {-# OVERLAPPABLE #-} Show c => Show (GameMap c) where
  show gm@(GameMap w h m) = show $ gm {gmMap = head . show <$> m}

mapTile :: c -> GameMap c -> (Int, Int) -> c
mapTile def gm p = fromMaybe def $ M.lookup p $ gmMap gm

inMap :: GameMap c -> (Int, Int) -> Bool
inMap (GameMap w h _) (x, y) = 0 <= x && x < w && 0 <= y && y < h

parse :: String -> GameMap Char
parse s = GameMap w h $ M.fromList [((x, y), c) | (y, line) <- zip [0 ..] l, (x, c) <- zip [0 ..] line, c /= '.']
  where
    l = lines s
    w = length $ head l
    h = length l

fromMap :: M.Map (Int, Int) c -> GameMap c
fromMap m = GameMap w h m
  where
    w = maximum $ fst <$> M.keys m
    h = maximum $ snd <$> M.keys m

-- Dijkstra's algorithm
shortestPath ::
  (Num n, Ord n, Ord state) =>
  state ->
  (state -> [state]) ->
  (state -> state -> n) ->
  (state -> Bool) ->
  Maybe (n, [state])
shortestPath initial nexts cost isGoal = case go M.empty iq M.empty of
  Nothing -> Nothing
  Just (target, dist, prevs) -> Just (dist, findpath prevs target)
  where
    iq = P.singleton 0 (initial, Nothing)
    findpath prevs goal = case prevs M.!? goal of
      Nothing -> [goal]
      Just cur -> cur : findpath prevs cur
    go nodes queue prevs = case P.getMin queue of
      Nothing -> Nothing
      Just (_, (cur, _)) | M.member cur nodes -> go nodes (P.deleteMin queue) prevs
      Just (curdist, (cur, prev))
        | isGoal cur -> Just (cur, curdist, prevs')
        | otherwise -> go nodes' queue' prevs'
        where
          dists s = (curdist + cost cur s, (s, Just cur))
          newentries = dists <$> filter (`M.notMember` nodes) (nexts cur)
          queue' = P.union queue $ P.fromList newentries
          nodes' = M.insert cur curdist nodes
          prevs' = case prev of
            Just p -> M.insert cur p prevs
            _ -> prevs