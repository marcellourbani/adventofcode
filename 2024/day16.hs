#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

{-# LANGUAGE PackageImports #-}

module Main where

import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.PQueue.Prio.Min qualified as P
import Linear.V2
import "containers" Data.Map.Strict qualified as M
import "containers" Data.Set qualified as S

data State = State {stLoc :: V2 Int, stDir :: V2 Int} deriving (Show, Eq, Ord)

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

parse :: String -> GameMap Char
parse s = GameMap w h $ M.fromList [(V2 x y, c) | (y, line) <- zip [0 ..] l, (x, c) <- zip [0 ..] line, c /= '.']
  where
    l = lines s
    w = length $ head l
    h = length l

type Input = GameMap Char

transitions :: GameMap Char -> State -> [(State, Int)]
transitions gm (State l d)
  | nt == '#' = rots
  | otherwise = (State (l + d) d, 1) : rots
  where
    nt = mapTile '.' gm $ l + d
    rots = [(State l $ rot90l d, 1000), (State l $ rot90r d, 1000)]
    rot90r (V2 a b) = V2 (-b) a
    rot90l (V2 a b) = V2 b (-a)

drawPath :: GameMap c -> M.Map (V2 Int) c -> GameMap c
drawPath gm m = gm {gmMap = M.union (gmMap gm) m}

drawPath2 :: GameMap c -> c -> [State] -> GameMap c
drawPath2 gm c l = drawPath gm $ M.fromList $ (,c) . stLoc <$> l

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

-- >>> solve $ parse "###############\n#.......#....E#\n#.#.###.#.###.#\n#.....#.#...#.#\n#.###.#####.#.#\n#.#.#.......#.#\n#.#.#####.###.#\n#...........#.#\n###.#.#####.#.#\n#...#.....#.#.#\n#.#.#.###.#.#.#\n#.....#...#.#.#\n#.###.#.#.#.#.#\n#S..#.....#...#\n###############"
-- (7036,45)

solve :: GameMap Char -> (Int, Int)
solve l = (p1, p2)
  where
    (_, p1, m) = head paths
    p2 = S.size $ S.fromList $ stLoc <$> M.keys m
    solutionMap = drawPath2 l 'O' $ M.keys m -- to show all touched locations
    paths = shortestPaths initial (transitions l) isGoal (const True)
    sl = head $ M.keys $ M.filter (== 'S') $ gmMap l
    initial = State sl $ V2 1 0
    isGoal (State p _) = mapTile '.' l p == 'E'

main :: IO ()
main = readFile "input/day16.txt" >>= print . solve . parse
