#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (find, foldl', minimumBy)
import Data.Function (on)
import Data.List (elemIndex)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.PQueue.Prio.Min as P
import qualified Data.Set as S

data ApTypes = A | B | C | D deriving (Show, Eq, Ord, Read, Enum)

type Coord = (Int, Int)

type BState = M.Map Coord ApTypes

type CandQueue = P.MinPQueue Int (BState, Int)

data Board = Board
  { minx :: Int,
    maxx :: Int,
    maxrow :: Int,
    current :: BState
  }
  deriving (Eq)

type BStateEntry = (Coord, ApTypes)

type Move = (BStateEntry, Coord)

data PossMove = PossMove {pmsrc :: Coord, pmdest :: Coord, pmcost :: Int, pmsteps :: S.Set Coord}
  deriving (Eq, Show)

type MovesMap = M.Map Coord [PossMove]

type Input = (Board, Board)

instance Show Board where
  show (Board minx maxx nr current) = "#############\n-- #" <> corridor <> "#\n" <> unlines (line <$> [2 .. nr]) <> "--   #########"
    where
      cell c = maybe "." show c
      corridor = concat $ cell <$> (M.lookup <$> zip [minx .. maxx] (repeat 1) <*> [current])
      line i
        | i == 2 = "-- " <> concat [c | x <- [0 .. 12], let c = if x >= 3 && even (x -3) && x < 10 then cell $ M.lookup (x, i) current else "#"]
        | otherwise = "--   " <> concat [c | x <- [2 .. 10], let c = if x >= 3 && even (x -3) then cell $ M.lookup (x, i) current else "#"]

parse :: String -> Input
parse i = (parseBoard i, parseBoard $ unlines $ take 3 l <> ["  #D#C#B#A#", "  #D#B#A#C#"] <> drop 3 l) where l = lines i

parseBoard :: String -> Board
parseBoard s = case lines s of
  (_ : c : ls) -> Board 1 (length c - 2) (length ls) (M.fromList (concat $ pl <$> zip [2 ..] (take (length ls -1) ls)))
  _ -> error "bad input"
  where
    pl (y, l) = rs <$> filter validC (zip (zip [0 .. 12] $repeat y) l)
    validC (_, c) = c `elem` "ABCD"
    rs (x, y) = (x, read [y])

unitScore :: ApTypes -> Int
unitScore x = 10 ^ fromEnum x

targetX :: ApTypes -> Int
targetX x = 3 + 2 * fromEnum x

moveCost :: Coord -> Coord -> Int
moveCost (x1, y1) (x2, y2) = abs (x2 - x1) + y2 + y1 -2

possMoves :: Board -> MovesMap
possMoves (Board minx maxx maxr _) = M.fromList $ coordPossMove <$> S.toList validCoords
  where
    homexs = S.fromList $ targetX <$> [A .. D]
    otherx = S.difference (S.fromList [minx .. maxx]) homexs
    homeCoords = [(x1, y1) | x1 <- S.toList homexs, y1 <- [2 .. maxr]]
    aisleCoords = [(x1, 1) | x1 <- S.toList otherx]
    validCoords = S.fromList $ homeCoords <> aisleCoords
    makemove src@(x1, y1) dst@(x2, y2) = PossMove src dst (moveCost src dst) validSteps
      where
        steps = zip (repeat x1) [y1, y1 -1 .. 1] <> zip [min x1 x2 .. max x1 x2] (repeat 1) <> zip (repeat x2) [1 .. y2]
        validSteps = S.intersection validCoords $ S.delete src $ S.fromList steps
    coordPossMove src@(x, y) = (src, makemove src <$> destinations)
      where
        destinations = case y of
          1 -> [(x1, y1) | x1 <- S.toList homexs, y1 <- [2 .. maxr]]
          _ -> [(x1, 1) | x1 <- S.toList otherx] ++ [(x1, y1) | x1 <- S.toList homexs, x1 /= x, y1 <- [2 .. maxr]]

estimate :: BState -> Int
estimate bs = sum $ M.mapWithKey go bs
  where
    go (x, y) a
      | x == targetX a = 0
      | otherwise = unitScore a * (abs (x - targetX a) + y)

simpleMove :: BState -> Move -> BState
simpleMove bs ((src, a), dst) = M.insert dst a $ M.delete src bs

boardMove :: Board -> Move -> Board
boardMove b m = b {current = simpleMove (current b) m}

successors :: Board -> MovesMap -> Int -> S.Set BState -> [Move] -> [(Int, (BState, Int, [Move]))]
successors b@(Board _ _ mr cur) mm curcost blacklist path = M.toList cur >>= go
  where
    occupied = M.keysSet cur
    isValid a (PossMove src@(x1, y1) dst@(x2, y2) _ steps) = case y2 of
      1 | x1 == targetX a -> freePath && not (fullbelow x1 y1 a)
      1 -> freePath
      _ -> x2 == targetX a && freePath && fullbelow x2 y2 a
      where
        freePath = S.empty == S.intersection occupied steps
        fullbelow xx yy aa = null [y | y <- [yy + 1 .. mr], M.lookup (xx, y) cur /= Just aa]

    applyMove a (PossMove src dst cst _) =
      if S.member nst blacklist
        then Nothing
        else Just (movecost + estimate nst, (nst, movecost, path ++ [((src, a), dst)]))
      where
        movecost = curcost + cst * unitScore a
        nst = simpleMove cur ((src, a), dst)

    go ((x, y), a) = catMaybes $ applyMove a <$> moves
      where
        moves = maybe [] (filter (isValid a)) (M.lookup (x, y) mm)

completed :: BState -> Bool
completed bs = all icomp $ M.toList bs where icomp ((x, y), a) = y /= 1 && x == targetX a

-- >>> solve $ parse "#############\n#...........#\n###B#C#B#D###\n  #A#D#C#A#\n  #########"
-- (12521,44169)

solve :: Input -> (Int, Int)
solve (i1, i2) = (go i1, go i2)
  where
    go i@(Board _ _ nr cm) = fst $aStar pmoves iq S.empty
      where
        iq = P.singleton 0 (cm, 0, [])
        pmoves = possMoves i
        aStar mm queue visited
          | completed cur = (curpr, path)
          | S.member cur visited = aStar mm queue'' visited
          | otherwise = aStar mm queue'' visited'
          where
            mi@(_, (cur, curpr, path)) = P.findMin queue
            nexts = successors i {current = cur} mm curpr visited path
            queue' = P.deleteMin queue
            queue'' = foldl' (flip (uncurry P.insert)) queue' nexts
            visited' = S.insert cur visited

main :: IO ()
main = readFile "input/day23.txt" >>= print . solve . parse
