#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

parse :: String -> [String]
parse = lines

data LineState = Complete String | Incomplete String Int | Corrupted String Char Int | Invalid deriving (Show)

parens = [('(', ')', 3), ('[', ']', 57), ('{', '}', 1197), ('<', '>', 25137)]

closingscores = M.fromList $ zip ")]}>" [1 ..]

-- >>> solve $ parse "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]"
-- (26397,288957)

solve :: [String] -> (Int, Int)
solve inp = (sum $linescore <$> linestates, middle)
  where
    linestates = lineState [] "" <$> inp
    middle = incompletescores !! div (length incompletescores) 2
    incompletescores = sort $ calcincompletescores [] linestates
    openings = M.fromList $ map (\(a, b, c) -> (a, (b, c))) parens
    scores = M.fromList $ map (\(_, b, c) -> (b, c)) parens
    score c = fromMaybe 0 $ M.lookup c scores
    closescore (_, (c, _)) = fromMaybe 0 $ M.lookup c closingscores
    linescore ls = case ls of
      Corrupted _ _ s -> s
      _ -> 0
    closeLineScore acc stack = case stack of
      [] -> acc
      (x : xs) -> closeLineScore (5 * acc + closescore x) xs
    calcincompletescores acc states = case states of
      [] -> acc
      (Incomplete _ s : xs) -> calcincompletescores (s : acc) xs
      (x : xs) -> calcincompletescores acc xs
    lineState stack parsed nsl = case (nsl, stack) of
      ([], []) -> Complete parsed
      (c : cs, (_, (cand, _)) : ss) ->
        if cand == c
          then lineState ss np cs
          else case M.lookup c openings of
            Nothing -> Corrupted parsed c $ score c
            Just p -> lineState ((c, p) : stack) np cs
        where
          np = parsed ++ [c]
      (c : cs, _) -> case M.lookup c openings of
        Nothing -> Corrupted parsed c $ score c
        Just p -> lineState ((c, p) : stack) (parsed ++ [c]) cs
      _ -> Incomplete parsed $ closeLineScore 0 stack

main :: IO ()
main = readFile "input/day10.txt" >>= print . solve . parse
