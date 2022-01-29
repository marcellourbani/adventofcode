-- stack --resolver lts-18.18 script

module Main where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.PQueue.Prio.Min as P
import qualified Data.Set as S

data Entry = Entry {ename :: String, isChip :: Bool} deriving (Show, Eq, Ord)

data State = State {selev :: Int, sgrid :: M.Map Entry Int} deriving (Eq, Ord)

type Move = (Int, Int, S.Set Entry)

minRow = 1

maxRow = 4

instance Show State where
  show (State e g) = "\n" <> unlines (sl <$> [maxRow, maxRow -1 .. minRow])
    where
      ks = S.toList $ S.fromList $ ename <$> M.keys g
      se l = if l == e then "E " else ". "
      sl l = "-- " <> show l <> " " <> se l <> concat [sc (Entry el c) l | el <- ks, c <- [False, True]]
      sc e@(Entry n ic) l = case M.lookup e g of
        Just x | x == l -> head n : if ic then "M " else "G "
        _ -> ".  "

parse :: String -> State
parse s = State minRow $ M.fromList entries
  where
    entries = zip [minRow ..] (lines (filter (`notElem` ",.") s)) >>= pl
    pl (y, a) = zip (pw <$> sp a) (repeat y)
    sp a = filter (`notElem` ["a", "and", "microchip", "generator", "relevant"]) $ (drop 5 . words) a
    sps a = (drop 5 . words) a
    pw a = case splitOn "-" a of
      [b, _] -> Entry b True
      _ -> Entry a False

isGoal :: State -> Bool
isGoal (State _ m) = all (== maxRow) m

nextStates :: State -> [(State, Move)]
nextStates s@(State e g) = nextels >>= moves
  where
    nextels = [x | x <- [max (e - 1) $ minimum g .. min maxRow $ e + 1], x /= e]
    curline = M.keys $ M.filter (== e) g
    candidates = S.fromList <$> ((: []) <$> curline) <> [[e1, e2] | e1 <- curline, e2 <- curline, e1 < e2]

    am i c = (applyMove s m, m) where m = (e, i, c)
    moves i = filter (valid . fst) $ am i <$> candidates
    valid (State _ gg) = null freecs
      where
        (cs, gs) = M.partitionWithKey (const . isChip) gg
        misplaced (Entry n _, i) = M.lookup (Entry n False) gs /= Just i && (not . null $ M.filter (== i) gs)
        freecs = filter misplaced $ M.toList cs

applyMove :: State -> Move -> State
applyMove (State _ g) m@(_, d, s) = State d $ M.union updated g
  where
    updated = M.fromList $ zip (S.toList s) (repeat d)

aStar :: Ord s => P.MinPQueue Int (s, Int, [m]) -> S.Set s -> (s -> Bool) -> (s -> Int -> [(Int, (s, Int, m))]) -> Maybe (s, Int, [m])
aStar queue visited goal nextStates
  | P.null queue = Nothing
  | goal curst = Just (curst, curcost, curpath)
  | S.member curst visited = aStar queue' visited goal nextStates
  | otherwise = aStar queue'' visited' goal nextStates
  where
    (_, (curst, curcost, curpath)) = P.findMin queue
    queue' = P.deleteMin queue
    visited' = S.insert curst visited
    nexts l = case l of
      [] -> []
      (e, (s, c, m)) : xs
        | S.member s visited' -> nexts xs
        | otherwise -> (e, (s, c, curpath <> [m])) : nexts xs
    queue'' = P.union queue' $ P.fromList $ nexts $ nextStates curst curcost

nextStatesCosted :: State -> Int -> [(Int, (State, Int, Move))]
nextStatesCosted s i = addCost <$> nextStates s
  where
    addCost (st, m) = (cost st, (st, i + 1, m))
    cost (State e g) = i + sum ((maxRow -) <$> g)

-- >>> solve  $ parse "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.\nThe second floor contains a hydrogen generator.\nThe third floor contains a lithium generator.\nThe fourth floor contains nothing relevant."
-- 11
solve :: State -> (Int, Int)
solve l = (p1, p2)
  where
    additional = Entry <$> ["dilithium", "elerium"] <*> [True, False]
    p2s = l {sgrid = M.union (sgrid l) $ M.fromList $ zip additional $ repeat 1}
    Just (_, p1, _) = aStar (P.singleton 0 (l, 0, [])) S.empty isGoal nextStatesCosted
    Just (_, p2, _) = aStar (P.singleton 0 (p2s, 0, [])) S.empty isGoal nextStatesCosted

main :: IO ()
main = readFile "input/day11.txt" >>= print . solve . parse
