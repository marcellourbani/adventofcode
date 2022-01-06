#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE TupleSections #-}

module Main where

import Data.Foldable (find, minimumBy)
import Data.Function (on)
import Data.List (elemIndex)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)

data ApTypes = A | B | C | D deriving (Show, Eq, Ord, Read, Enum)

type Location = (Int, Int)

type ApLocations = M.Map Location ApTypes

data Board = Board
  { minx :: Int,
    maxx :: Int,
    current :: ApLocations,
    score :: Int
  }
  deriving (Eq)

type BoardEntry = (Location, ApTypes)

type Move = (BoardEntry, Location)

type Input = Board

instance Show Board where
  show (Board minx maxx current score) = "#############\n-- #" <> corridor <> "#\n-- " <> line1 <> "\n--   " <> line2 <> "\n--   #########\n-- " <> show score
    where
      cell c = maybe "." show c
      corridor = concat $ cell <$> (M.lookup <$> zip [minx .. maxx] (repeat 1) <*> [current])
      line1 = concat [c | x <- [0 .. 12], let c = if x >= 3 && even (x -3) && x < 10 then cell $ M.lookup (x, 2) current else "#"]
      line2 = concat [c | x <- [2 .. 10], let c = if x >= 3 && even (x -3) then cell $ M.lookup (x, 3) current else "#"]

parse :: String -> Input
parse s = case lines s of
  (_ : c : l1 : l2 : _) -> Board 1 (length c - 2) (M.fromList (pl 2 l1 ++ pl 3 l2)) 0
  _ -> error "bad input"
  where
    pl y l = rs <$> filter validC (zip (zip [0 .. 12] $repeat y) l)
    validC (_, c) = c `elem` "ABCD"
    rs (x, y) = (x, read [y])

unitScore :: ApTypes -> Int
unitScore x = 10 ^ fromEnum x

targetX :: ApTypes -> Int
targetX x = 3 + 2 * fromEnum x

moveCost :: Move -> Int
moveCost (((x1, y1), a), (x2, y2)) = unitScore a * (abs (x2 - x1) + abs (y2 - y1))

doMove :: Board -> Move -> Board
doMove b@(Board _ _ cur sc) m@((src, ap), dest) = b {score = sc + moveCost m, current = M.insert dest ap $ M.delete src cur}

completed :: Board -> Bool
completed (Board _ _ cur _) = all ve $ M.toList cur where ve ((x, y), a) = x == targetX a && (y /= 1)

needsMoving :: Board -> BoardEntry -> Bool
needsMoving b@(Board _ _ cur _) (p, ap) =
  case p of
    (_, 1) -> True
    (x, 3) -> x == targetX ap
    (x, _) -> M.lookup (x, 3) cur == Just ap

validMoves :: Board -> (Location, ApTypes) -> [Move]
validMoves (Board lx hx m _) st@((x, y), ap) = zip (repeat st) $ case y of
  3 -> if occupied 2 x then [] else cdests x
  2 -> cdests x
  _ -> case (cell tx 3, cell tx 2) of
    (Nothing, Nothing) -> [(tx, 3)]
    (Just a, Nothing) | a == ap -> [(tx, 2)]
    _ -> []
    where
      tx = targetX ap
  where
    cdests a = zip (filter (/= a) [lb a .. ub a]) (repeat 1)
    lb a = case (a < lx, occupied 1 a) of
      (True, _) -> lx
      (False, True) -> a + 1
      _ -> lb (a -1)
    ub a = case (a > hx, occupied 1 a) of
      (True, _) -> hx
      (False, True) -> a - 1
      _ -> ub (a + 1)
    cell a b = M.lookup (a, b) m
    occupied i = flip (M.member . (,i)) m

allValidMoves :: Board -> [Move]
allValidMoves b = M.toList (current b) >>= validMoves b

-- >>> a = parse "#############\n#...........#\n###B#C#B#D###\n  #A#D#C#A#\n  #########"
-- >>> length $ allValidMoves a
-- >>> b=doMove a (((7,2),B),(4,1))
-- >>> validMoves b ((9,2),D)
-- >>> b
-- 40
-- [(((9,2),D),(5,1)),(((9,2),D),(6,1)),(((9,2),D),(7,1)),(((9,2),D),(8,1)),(((9,2),D),(10,1)),(((9,2),D),(11,1))]
-- #############
-- #...B.......#
-- ###B#C#.#D###
--   #A#D#C#A#
--   #########
-- 40

-- >>> solve $ parse "#############\n#...........#\n###B#C#B#D###\n  #A#D#C#A#\n  #########"
-- ProgressCancelledException

-- >>> solve $ parse "#############\n#...........#\n###B#C#B#D###\n  #A#B#C#D#\n  #########"
-- ProgressCancelledException

-- solve :: Input -> (Int, Int)
solve i@(Board _ _ cm _) = go $ M.singleton cm (0, False)
  where
    go cands = case nextPivots of
      [] -> 0
      _ -> case goal of
        Just (Board _ _ _ score) -> score
        Nothing -> go cands'
        where
          (cur, (sc, _)) = minimumBy (on compare (fst . snd)) nextPivots
          curb = i {current = cur, score = sc}
          nexts = filter (flip M.notMember cands . current) $ doMove curb <$> allValidMoves curb
          toEntry (Board _ _ m s) = (m, (s, False))
          goal = find completed nexts
          cands' = M.union (M.insert cur (sc, True) cands) $ M.fromList $ toEntry <$> nexts
      where
        nextPivots = filter (not . snd . snd) $ M.toList cands

main :: IO ()
main = readFile "input/day23.txt" >>= print . solve . parse
