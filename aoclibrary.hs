-- couldn't find a way to import this as a module in stack scripts, will just copy and paste
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module AocLibrary (GameMap, mapTile) where

import Control.Lens ((^.))
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified Data.PQueue.Prio.Min as P
import Linear.V2
import qualified "containers" Data.Map.Strict as M
import qualified "containers" Data.Set as S

data GameMap c = GameMap {gmW :: Int, gmH :: Int, gmMap :: M.Map (V2 Int) c} deriving (Eq)

instance Functor GameMap where
  fmap f (GameMap w h m) = GameMap w h $ M.map f m

instance Show (GameMap Char) where
  show gm@(GameMap w h m) = unlines [[mapTile '.' gm (V2 x y) | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]

instance {-# OVERLAPPABLE #-} (Show c) => Show (GameMap c) where
  show gm@(GameMap w h m) = show $ gm {gmMap = head . show <$> m}

mapTile :: c -> GameMap c -> V2 Int -> c
mapTile def gm p = fromMaybe def $ M.lookup p $ gmMap gm

inMap :: GameMap c -> V2 Int -> Bool
inMap (GameMap w h _) (V2 x y) = 0 <= x && x < w && 0 <= y && y < h

drawPath :: Bool -> GameMap c -> M.Map (V2 Int) c -> GameMap c
drawPath drawover gm m = gm {gmMap = if drawover then M.union m (gmMap gm) else M.union (gmMap gm) m}

parse :: String -> GameMap Char
parse s = GameMap w h $ M.fromList [(V2 x y, c) | (y, line) <- zip [0 ..] l, (x, c) <- zip [0 ..] line, c /= '.']
  where
    l = lines s
    w = length $ head l
    h = length l

fromMap :: M.Map (V2 Int) c -> GameMap c
fromMap m = GameMap w h m
  where
    w = 1 + maximum ((^. _x) <$> M.keys m)
    h = 1 + maximum ((^. _y) <$> M.keys m)

decodeDir :: Char -> V2 Int
decodeDir c = case c of
  '>' -> V2 1 0
  '<' -> V2 (-1) 0
  '^' -> V2 0 (-1)
  'v' -> V2 0 1

encodeDir :: V2 Int -> Char
encodeDir v = case v of
  V2 1 0 -> '>'
  V2 (-1) 0 -> '<'
  V2 0 (-1) -> '^'
  V2 0 1 -> 'v'

rot90r :: (Num a) => V2 a -> V2 a -- 90^ right, with y going down
rot90r (V2 a b) = V2 (-b) a

rot90l :: (Num a) => V2 a -> V2 a -- 90^ left, with y going down
rot90l (V2 a b) = V2 b (-a)

keysOf :: (Eq c) => GameMap c -> c -> [V2 Int]
keysOf gm g = M.keys $ M.filter (== g) $ gmMap gm

directions :: [V2 Int]
directions = (V2 0 <$> [-1, 1]) <> (V2 <$> [-1, 1] <*> [0])

-- Dijkstra's algorithm - single solution
shortestPath :: (Num n, Ord n, Ord state) => state -> (state -> [(state, n)]) -> (state -> Bool) -> Maybe (n, [state])
shortestPath initial nexts isGoal = case go M.empty iq M.empty of
  Nothing -> Nothing
  Just (target, dist, prevs) -> Just (dist, target : findpath prevs target)
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
          dists (s, cost) = (curdist + cost, (s, Just cur))
          newentries = dists <$> filter ((`M.notMember` nodes) . fst) (nexts cur)
          queue' = P.union queue $ P.fromList newentries
          nodes' = M.insert cur curdist nodes
          prevs' = case prev of
            Just p -> M.insert cur p prevs
            _ -> prevs

-- Dijkstra's algorithm - all solutions
expandPaths :: (Num n, Ord n, Ord state) => (state, n, M.Map state (n, S.Set state)) -> [(n, [state])]
expandPaths (s, n, m) = go (s, [s])
  where
    prevs st = maybe [] (S.toList . snd) $ M.lookup st m
    go (st, l) = case ps of
      [] -> [(n, l)]
      _ -> nxs >>= go
      where
        ps = prevs st
        nxs = zip ps $ (: l) <$> ps

shortestPaths :: (Num n, Ord n, Ord state) => state -> (state -> [(state, n)]) -> (state -> Bool) -> ([(state, n, M.Map state (n, S.Set state))] -> Bool) -> [(state, n, M.Map state (n, S.Set state))]
shortestPaths initial nexts isGoal stopSearch = solutions
  where
    solutions = go iq M.empty Nothing []
    iq = P.singleton 0 (initial, Nothing)
    joinSources (newdist, newset) (olddist, oldset)
      | newdist == olddist = (olddist, S.union newset oldset)
      | otherwise = (olddist, oldset)
    -- remove states not included in any solutions
    relevants targ nodes = relevants' [targ] nodes $ M.restrictKeys nodes $ S.singleton targ
    relevants' targs nodes acc = case targs' of
      [] -> acc
      _ -> relevants' targs' nodes acc'
      where
        prevs = S.unions $ snd <$> mapMaybe (`M.lookup` nodes) targs
        targs' = filter (`M.notMember` acc) $ S.toList prevs
        intarg k _ = k `elem` targs'
        acc' = M.union acc $ M.filterWithKey intarg nodes
    go queue nodes minD acc = case P.getMin queue of
      Nothing -> acc
      Just (curdist, (cur, prev))
        | tooLong -> acc
        | M.member cur nodes -> go queue' nodes' minD acc
        | isGoal cur && stopSearch acc' -> acc'
        | isGoal cur -> go queue' nodes (Just curdist) acc' -- goal might not be unique
        | otherwise -> go queue'' nodes' minD acc
        where
          acc' = (cur, curdist, relevants cur nodes') : acc
          nodes' = M.insertWith joinSources cur (curdist, S.fromList $ catMaybes [prev]) nodes
          dists (s, cost) = (curdist + cost, (s, Just cur))
          newentries = dists <$> filter ((`M.notMember` nodes') . fst) (nexts cur)
          queue' = P.deleteMin queue
          queue'' = P.union queue' $ P.fromList newentries
          tooLong = maybe False (< curdist) minD
