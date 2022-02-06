#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (find)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as S

data Prog = Prog {pname :: String, pweight :: Int, pchildren :: S.Set String} deriving (Show, Eq)

parse :: String -> [Prog]
parse s = pl <$> lines (filter (`notElem` ",()") s)
  where
    pl l = case words <$> splitOn " -> " l of
      [[n, w], b] -> Prog n (read w) $ S.fromList b
      [[n, w]] -> Prog n (read w) S.empty
      _ -> error "parse error"

-- >>> solve $ parse "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)"
-- ("tknk",60)

solve :: [Prog] -> (String, Int)
solve l = (root, fromMaybe 0 $ go =<< M.lookup root nodemap)
  where
    nodemap = M.fromList $ zip (pname <$> l) l
    root = head $ S.toList $ S.difference names allchildren
    allchildren = S.unions $ pchildren <$> l
    names = S.fromList $ pname <$> l
    children (Prog _ _ ch) = catMaybes $ M.lookup <$> S.toList ch <*> [nodemap]
    totalWeight n = pweight n + sum (totalWeight <$> children n)
    balanced n = null cw || minimum cw == maximum cw where cw = totalWeight <$> children n
    badChild n = pweight n' - w' + goal
      where
        Just (n', w') = find ((/= goal) . snd) (zip cs weights)
        cs = children n
        weights = totalWeight <$> cs
        mins = length $ filter (== minimum weights) weights
        goal = if mins /= 1 then minimum weights else maximum weights
        cond = (if mins == 1 then (== minimum weights) else (== maximum weights)) . snd
    go n
      | balanced n = Nothing
      | null cs = Just $ badChild n
      | otherwise = Just n'
      where
        cs = filter (not . balanced) $ children n
        n' = head $ catMaybes $ go <$> cs

main :: IO ()
main = readFile "input/day07.txt" >>= print . solve . parse
