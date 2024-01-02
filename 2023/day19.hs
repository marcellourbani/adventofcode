#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module Main where

import Data.Either (fromRight)
import Data.Functor (($>))
import Data.List (partition)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, empty, many, manyTill, runParser, some, try, (<|>))
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char as LC
import qualified Text.Megaparsec.Char.Lexer as L

type Part = M.Map String Int

type PartRange = M.Map String (Int, Int)

data Destination = A | R | W String deriving (Show, Eq, Ord)

data Rule = Rule {ruKey :: String, ruLt :: Bool, ruValue :: Int, ruDest :: Destination} deriving (Show)

data Workflow = Workflow {wfName :: String, wfRules :: [Rule], wfDefault :: Destination} deriving (Show)

type Parser = Parsec Void String

sc :: Parser () -- consume spaces
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parseDest :: Parser Destination
parseDest = (symbol "A" $> A) <|> (symbol "R" $> R) <|> (W <$> many LC.alphaNumChar)

parseRule :: Parser Rule
parseRule = do
  component <- many LC.alphaNumChar
  s <- (symbol "<" $> True) <|> (symbol ">" $> False)
  n <- L.decimal
  _ <- symbol ":"
  Rule component s n <$> parseDest

parseWorkflow :: Parser Workflow
parseWorkflow = Workflow <$> many LC.alphaNumChar <*> (symbol "{" *> many (try parseRule <* symbol ",")) <*> (parseDest <* symbol "}")

parseList :: Parser a -> Parser [a]
parseList p = many (try (p <* symbol ",") <|> p)

parsePart :: Parser Part
parsePart = M.fromList <$> lexeme (char '{' *> parseList ((,) <$> many LC.alphaNumChar <*> (symbol "=" *> L.decimal)) <* char '}')

parse :: String -> (M.Map String Workflow, [Part])
parse s = (M.fromList $ zip (wfName <$> wfs) wfs, pl parsePart ps)
  where
    [wf, ps] = splitOn "\n\n" s
    wfs = pl parseWorkflow wf
    pl p i = fromRight [] $ runParser (many p) "" i

isAccepted :: M.Map String Workflow -> Part -> Bool
isAccepted wfs part = go $ W "in"
  where
    dest rs def = case rs of
      [] -> def
      Rule n True v d : _ | part M.! n < v -> d
      Rule n False v d : _ | part M.! n > v -> d
      _ : rs' -> dest rs' def
    go d = case d of
      A -> True
      R -> False
      W n -> case M.lookup n wfs of
        Nothing -> False
        Just (Workflow _ rs def) -> go $ dest rs def

-- valid but unnecessary
prune :: M.Map String Workflow -> M.Map String Workflow
prune wfs = toMap $ go seed wflist
  where
    toMap ws = M.fromList $ zip (wfName <$> ws) ws
    seed = S.fromList [A, W "in"]
    wflist = snd <$> M.toList wfs
    canReach targs (Workflow n rs def)
      | S.member (W n) targs = False
      | otherwise = any (`S.member` targs) (def : (ruDest <$> rs))
    convertRule targets r@(Rule _ _ _ d)
      | d `S.member` targets = r
      | otherwise = r {ruDest = R}
    convert targets (Workflow n rs def)
      | W n `S.member` targets = [Workflow n (convertRule targets <$> rs) def]
      | otherwise = []
    go targets ws
      | null toAdd = wflist >>= convert targets
      | otherwise = go targets' ws'
      where
        (toAdd, ws') = partition (canReach targets) ws
        targets' = S.union targets $ S.fromList (W . wfName <$> toAdd)

validPR :: PartRange -> Bool
validPR pr = and $ v <$> pr where v (a, b) = a <= b

splitPR :: PartRange -> Rule -> (Maybe (PartRange, Destination), Maybe PartRange)
splitPR pr (Rule partid lt v d)
  | d == R = (Nothing, rest)
  | otherwise = (accepted, rest)
  where
    (a, b) = pr M.! partid
    ifValid pr' = if validPR pr' then Just pr' else Nothing
    (accepted, rest)
      | lt = ((,d) <$> ifValid (M.insert partid (a, min b (v - 1)) pr), ifValid $ M.insert partid (max a v, b) pr)
      | otherwise = ((,d) <$> ifValid (M.insert partid (max a (v + 1), b) pr), ifValid $ M.insert partid (a, min b v) pr)

acceptableRanges :: M.Map String Workflow -> [PartRange]
acceptableRanges wfs = go [initial]
  where
    initial = (M.fromList $ (,(1, 4000)) <$> ["x", "m", "a", "s"], W "in")
    splitAll pr rs d = case rs of
      [] -> [(pr, d)]
      r : rs' -> case splitPR pr r of
        (Nothing, Nothing) -> []
        (Nothing, Just pr') -> splitAll pr' rs' d
        (Just p, Nothing) -> [p]
        (Just p, Just pr') -> p : splitAll pr' rs' d
    go seeds = case seeds of
      [] -> []
      (_, R) : ss -> go ss
      (pr, A) : ss -> pr : go ss
      (pr, W w) : ss -> case M.lookup w wfs of
        Nothing -> go ss
        Just (Workflow _ rs d) -> go (splitAll pr rs d <> ss)

-- >>> solve $ parse "px{a<2006:qkq,m>2090:A,rfg}\npv{a>1716:R,A}\nlnx{m>1548:A,A}\nrfg{s<537:gd,x>2440:R,A}\nqs{s>3448:A,lnx}\nqkq{x<1416:A,crn}\ncrn{x>2662:A,R}\nin{s<1351:px,qqz}\nqqz{s>2770:qs,m<1801:hdj,R}\ngd{a>3333:R,R}\nhdj{m>838:A,pv}\n\n{x=787,m=2655,a=1222,s=2876}\n{x=1679,m=44,a=2067,s=496}\n{x=2036,m=264,a=79,s=2244}\n{x=2461,m=1339,a=466,s=291}\n{x=2127,m=1623,a=2188,s=1013}"
-- (19114,167409079868000)

solve :: (M.Map String Workflow, [M.Map String Int]) -> (Int, Int)
solve (wfs, ps) = (p1, p2)
  where
    p1 = sum $ sum <$> filter (isAccepted wfs) ps
    score pr = product $ (+ 1) . (uncurry . flip) (-) <$> pr
    p2 = sum $ score <$> acceptableRanges (prune wfs)

main :: IO ()
main = readFile "input/day19.txt" >>= print . solve . parse
