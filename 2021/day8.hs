#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Foldable (find)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Set as S

type Input = ([S.Set Char], [S.Set Char])

numbers :: M.Map Integer (S.Set Char)
numbers = M.fromList $ zip [0 ..] $ S.fromList <$> ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

parse :: String -> [Input]
parse = map (parseLine . map words . splitOn "|") . lines
  where
    parseLine [a, b] = (S.fromList <$> a, S.fromList <$> b)
    parseLine _ = error "Invalid input"

rewire :: [S.Set Char] -> M.Map (S.Set Char) Char
rewire l = M.fromList numbers
  where
    segmentByLen s l = fromJust $ find ((== l) . S.size) s
    segmentsByLen s l = filter ((== l) . S.size) s
    segmentByLenDiff s e l = fromJust $ find ((== l) . S.size . S.difference e) s

    zero = segmentByLenDiff sixsegments (S.difference four one) 1
    one = segmentByLen l 2
    two = segmentByLenDiff (filter (/= three) fivesegments) six 2
    three = segmentByLenDiff fivesegments seven 0
    four = segmentByLen l 4
    five = segmentByLenDiff fivesegments six 1
    six = segmentByLenDiff sixsegments one 1
    seven = segmentByLen l 3
    eight = segmentByLen l 7
    nine = head [s | s <- sixsegments, s /= zero, s /= six]
    sixsegments = segmentsByLen l 6
    fivesegments = segmentsByLen l 5
    numbers = zip [zero, one, two, three, four, five, six, seven, eight, nine] ['0' ..]

sample =
  "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\n\
  \edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\n\
  \fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\n\
  \fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\n\
  \aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\n\
  \fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\n\
  \dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\n\
  \bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\n\
  \egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\n\
  \gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"

-- >>> solve $ parse sample
-- (26,61229)

solve :: [Input] -> (Int, Int)
solve list = (sum $ length . unique <$> list, sum $ read . convertLine <$> list)
  where
    isUnique s = S.member (S.size s) (S.fromList [2, 4, 3, 7])
    unique (_, v) = filter isUnique v
    convertLine (patterns, values) = fromMaybe 'k' <$> (M.lookup <$> values <*> pure transcode)
      where
        transcode = rewire patterns

main :: IO ()
main = readFile "input/day8.txt" >>= print . solve . parse
