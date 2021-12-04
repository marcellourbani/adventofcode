#!/usr/bin/env stack
-- stack --resolver lts-10.2 script
module Main where

import           Data.List.Split
import           Data.Map        (empty, fromList, keys, member, (!))
import qualified Data.Set        as S

testCase :: [Char]
testCase = "light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags."
-- >>> solve testCase
-- (4,32)
solve s = (S.size $ containers "shiny gold",allBags "shiny gold" - 1) where
  parse = fromList. fmap (parseBag.splitOn " bags contain ") . lines
  parseSingle (n:c1:c2:_) = (c1++" "++c2,read n ::Int)
  parseBag (color:rest:_) = (color,bagcont) where
      bagcont
        | rest == "no other bags." = empty
        | otherwise = fromList $ parseSingle.words <$> splitOn ", " rest
  contents = parse s
  parents = fromList [(x,S.fromList [y|y<-keys contents,member x $ contents ! y ])|x<-keys contents]
  containers s = go S.empty [s] where
    go acc keys
      | null newe = acc
      | otherwise = go (S.union acc newe) (S.toList newe)
      where newe = S.difference (S.unions $ (parents !) <$> keys) acc
  allBags b
    | null level = 1
    | otherwise = 1 + sum ( bagContent <$> keys level)
    where level = contents ! b
          bagContent = \k -> level ! k * allBags k





main :: IO ()
main = readFile "input/day7.txt" >>= print.solve
