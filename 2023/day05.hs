#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.List (find, partition, sort)
import Data.List.Split (splitOn)

data TranslationMap = TranslationMap {tmSource :: String, tmTarget :: String, tmRules :: [(Int, Int, Int)]} deriving (Show)

data GameInput = GameInput {giSeeds :: [Int], giTranslationMaps :: [TranslationMap]} deriving (Show)

data Segment = Segment {sStart :: Int, sEnd :: Int} deriving (Show, Eq, Ord)

parse :: [Char] -> GameInput
parse s = GameInput seeds $ parseTm <$> t
  where
    h : t = splitOn "\n\n" s
    seeds = read <$> tail (words h)
    parseTm tm = TranslationMap ms md $ parseR <$> ls
      where
        mh : ls = lines tm
        [ms, md] = splitOn "-to-" $ head $ words mh
        parseR r = (a, b, c) where [a, b, c] = read <$> words r

convertStuff :: GameInput -> (String, [Int]) -> Maybe (String, [Int])
convertStuff (GameInput _ tms) (inp, vs) = case find ((== inp) . tmSource) tms of
  Nothing -> Nothing
  Just tm -> Just (tmTarget tm, translate (tmRules tm) <$> vs)
    where
      translate rules v = case rules of
        [] -> v
        (a, b, c) : rs
          | v >= b && v < b + c -> v + a - b
          | otherwise -> translate rs v

joinSegments :: [Segment] -> [Segment]
joinSegments segs = go $ sort segs
  where
    go s = case s of
      Segment a b : Segment c d : rs
        | c <= b -> joinSegments (Segment a (max b d) : rs)
        | otherwise -> Segment a b : joinSegments (Segment c d : rs)
      _ -> s

intersect :: Segment -> Segment -> [(Segment, Bool)]
intersect s1@(Segment a b) s2@(Segment c d)
  | b < c = [(s1, False)]
  | a > d = [(s1, False)]
  | a < c && b > d = [(Segment a $ c - 1, False), (s2, True), (Segment (d + 1) b, False)]
  | a >= c && b <= d = [(s1, True)]
  | a < c && b >= c && b <= b = [(Segment a $ c - 1, False), (Segment c b, True)]
  | a >= c && b > d = [(Segment a d, True), (Segment (d + 1) b, False)]

convertSegments :: [Segment] -> [(Segment, Int)] -> [Segment]
convertSegments sources trules = case trules of
  [] -> sources
  (r@(Segment startP _), destP) : rs -> translated <> convertSegments rest rs
    where
      transl (Segment a b) = Segment (a - startP + destP) $ b - startP + destP
      (totransl, unmodified) = partition snd $ sources >>= flip intersect r
      translated = transl . fst <$> totransl
      rest = fst <$> unmodified

translateSegments :: [Segment] -> TranslationMap -> [Segment]
translateSegments segs (TranslationMap _ _ rules) = simplified
  where
    tr (d, st, l) = (Segment st (st + l - 1), d)
    raw = convertSegments segs $ tr <$> rules
    simplified = joinSegments $ sort raw
    extract (Segment a b) = [a, b - a + 1]
    simplify segs = case segs of
      Segment a b : Segment c d : rs
        | c <= b -> simplify (Segment a (max b d) : rs)
        | otherwise -> Segment a b : simplify (Segment c d : rs)
      _ -> segs

convertSegStuff :: GameInput -> (String, [Segment]) -> Maybe (String, [Segment])
convertSegStuff (GameInput _ tms) (inp, vs) = case find ((== inp) . tmSource) tms of
  Nothing -> Nothing
  Just tm -> Just (tmTarget tm, translateSegments vs tm)

-- >>> solve $ parse "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4"
-- (35,46)
solve l = (p1 ("seed", giSeeds l), p2 ("seed", numToSeg $ giSeeds l))
  where
    p1 s@(k, nums)
      | k == "location" = minimum nums
      | otherwise = maybe (-1) p1 $ convertStuff l s
    numToSeg nums = case nums of
      (st : l : rest) -> Segment st (st + l - 1) : numToSeg rest
      _ -> []
    p2 s@(k, nums)
      | k == "location" = sStart $ minimum nums
      | otherwise = maybe (-1) p2 $ convertSegStuff l s

main :: IO ()
main = readFile "input/day05.txt" >>= print . solve . parse
