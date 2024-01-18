#!/usr/bin/env stack
-- stack --resolver lts-18.18 script --optimize

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import Data.List (nub)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Random (StdGen, mkStdGen, random)

data Node = Node {nId :: String, nOut :: S.Set String, nAlias :: S.Set String} deriving (Show, Eq)

type Graph = M.Map String Node

parse :: String -> M.Map String Node
parse s = M.fromList $ parseLine . words <$> lines (filter (/= ':') s)
  where
    parseLine (x : xs) = (x, Node x (S.fromList xs) (S.singleton x))

addReverse :: Graph -> Graph
addReverse m = M.unionWith merge m reverse
  where
    reverse = M.unionsWith merge [M.singleton v $ Node v (S.singleton k) (S.singleton v) | (k, es) <- M.toList m, v <- S.toList (nOut es)]
    merge (Node i1 s1 k1) (Node i2 s2 k2) | i1 == i2 = Node i1 (S.union s1 s2) (S.union k1 k2)

collapse :: (Node, Node) -> Node
collapse (Node i1 s1 k1, Node i2 s2 k2) = Node i1 (S.filter (`notElem` [i1, i2]) (S.union s1 s2)) (S.union k1 k2)

contractByOne :: Graph -> (Node, Node) -> Graph
contractByOne g ns@(Node i1 _ _, Node i2 _ _) = M.insert i1 (collapse ns) g'
  where
    toIn i = if i == i2 then i1 else i
    removeOut e = e {nOut = S.map toIn $ nOut e}
    g' = removeOut <$> M.delete i2 g

selectNodes :: StdGen -> Graph -> ((Node, Node), StdGen)
selectNodes s g = ((n1, n2), s'')
  where
    (i, s') = random s
    (io, s'') = random s'
    n1 = snd $ M.elemAt (mod i $ M.size g) g
    ns = nOut n1
    n2 = g M.! S.elemAt (mod io $ S.size ns) ns

contract :: StdGen -> Graph -> ((Node, Node), StdGen)
contract s g
  | M.size g == 2 = (extract $ snd <$> M.toList g, s)
  | otherwise = contract s' g'
  where
    (es, s') = selectNodes s g
    g' = contractByOne g es
    extract [e1, e2] = (e1, e2)

-- Karger's algorithm https://en.wikipedia.org/wiki/Karger%27s_algorithm
-- plus stop at a given target
-- it's not deterministic, so I'll keep trying until connection length equals target
minimumCut :: Graph -> Int -> ((Node, Node), [(String, String)], Int)
minimumCut graph target = go seed graph 1
  where
    seed = mkStdGen 0
    connections (Node _ o1 a1, Node _ o2 a2) = nub [(i, o) | i <- S.toList a1, o <- S.toList $ S.intersection a2 (nOut $ graph M.! i)]
    go s g n
      | length conns == target = (ns, conns, n)
      | otherwise = go s' g (n + 1)
      where
        (ns, s') = contract s g
        conns = connections ns

-- >>> solve $ parse "jqt: rhn xhk nvd\nrsh: frs pzl lsr\nxhk: hfx\ncmg: qnr nvd lhk bvb\nrhn: xhk bvb hfx\nbvb: xhk hfx\npzl: lsr hfx nvd\nqnr: nvd\nntq: jqt hfx bvb xhk\nnvd: lhk\nlsr: lhk\nrzs: qnr cmg lsr rsh\nfrs: qnr lhk lsr"
-- (54,34)

solve :: Graph -> (Int, Int)
solve l = (p1, p2)
  where
    comp = addReverse l
    ((n1, n2), _, n) = minimumCut comp 3
    p1 = S.size (nAlias n1) * S.size (nAlias n2)
    p2 = n

main :: IO ()
main = readFile "input/day25.txt" >>= print . solve . parse
