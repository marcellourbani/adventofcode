#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

data Tile = Sand | Rock deriving (Show, Eq)

newtype WM = WM {unWM :: M.Map (Int, Int) Tile}

instance Show WM where
  show (WM m) = show (minx, miny) <> show (maxx, maxy) <> "\n" <> unlines (line <$> [miny .. maxy])
    where
      ks = M.keys m
      minx = minimum $ fst <$> ks
      maxx = maximum $ fst <$> ks
      miny = minimum $ snd <$> ks
      maxy = maximum $ snd <$> ks
      ch c = case c of
        Nothing -> '.'
        Just Rock -> '#'
        Just Sand -> 'o'
      line l = [c | x <- [minx .. maxx], let c = ch $ M.lookup (x, l) m]

parse :: String -> [[(Int, Int)]]
parse s = pl <$> lines s
  where
    pl l = pi <$> splitOn " -> " l
    pi l = (read a, read b) where [a, b] = splitOn "," l

createMap :: [[(Int, Int)]] -> WM
createMap di = WM $ M.fromList $ di >>= dline
  where
    dline l = zip l (tail l) >>= uncurry dsegment
    dsegment (x1, y1) (x2, y2)
      | x1 == x2 = [((x1, y), Rock) | y <- [min y1 y2 .. max y1 y2]]
      | y1 == y2 = [((x, y1), Rock) | x <- [min x1 x2 .. max x1 x2]]
      | otherwise = undefined

fillMap :: WM -> Bool -> WM
fillMap (WM m) part2 = WM m'
  where
    maxy = maximum $ snd <$> M.keys m
    m' = if part2 then M.insert (500, 0) Sand $ go m else go m
    go cm = maybe cm go $ nextp (500, 0) cm
    nextp (x, y) cm
      | y > maxy && not part2 = Nothing
      | otherwise = case (dn, dl, dr) of
        (Just _, Just _, Just _)
          | y == 0 -> Nothing
          | otherwise -> Just $ M.insert (x, y) Sand cm
        (Nothing, _, _) -> nextp (x, y + 1) cm
        (Just _, Nothing, _) -> nextp (x -1, y + 1) cm
        (Just _, Just _, Nothing) -> nextp (x + 1, y + 1) cm
      where
        lu (a, b)
          | part2 && b == maxy + 2 = Just Rock
          | otherwise = M.lookup (a, b) cm
        dn = lu (x, y + 1)
        dl = lu (x -1, y + 1)
        dr = lu (x + 1, y + 1)

-- >>> solve $ parse "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9"
-- (24,93)

solve :: [[(Int, Int)]] -> (Int, Int)
solve l = (p1, p2)
  where
    p1 = M.size $ M.filter (== Sand) $ unWM (fillMap initial False)
    p2 = M.size $ M.filter (== Sand) $ unWM (fillMap initial True)
    initial = createMap l

main :: IO ()
main = readFile "input/day14.txt" >>= print . solve . parse
