#!/usr/bin/env stack
-- stack --resolver lts-12.21 script

{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad.Combinators (choice)
import Data.Either (fromRight)
import qualified Data.Set as S
import Data.Void
import Text.Megaparsec (MonadParsec (try), Parsec, many, runParser)
import Text.Megaparsec.Char

data Direction = Direction {dirE :: Int, dirN :: Int} deriving (Eq, Show, Ord)

type Parser = Parsec Void String

parseLine :: Parser [Direction]
parseLine = many parseDir
  where
    parseDir = choice $ try <$> [parseNE, parseNW, parseSE, parseSW, parseE, parseW]
    parseE = Direction 2 0 <$ string "e"
    parseW = Direction (-2) 0 <$ string "w"
    parseNE = Direction 1 1 <$ string "ne"
    parseNW = Direction (-1) 1 <$ string "nw"
    parseSE = Direction 1 (-1) <$ string "se"
    parseSW = Direction (-1) (-1) <$ string "sw"

fromReference :: [Direction] -> Direction
fromReference = foldl addDir (Direction 0 0)
  where
    addDir (Direction e1 n1) (Direction e2 n2) = Direction (e1 + e2) (n1 + n2)

adjacents :: Direction -> S.Set Direction
adjacents d = S.fromList $ fromReference . (: [d]) <$> deltas
  where
    deltas = uncurry Direction <$> [(e, n) | e <- [-2 .. 2], n <- [-1 .. 1], abs e + abs n == 2]

-- >>> solve "sesenwnenenewseeswwswswwnenewsewsw\nneeenesenwnwwswnenewnwwsewnenwseswesw\nseswneswswsenwwnwse\nnwnwneseeswswnenewneswwnewseswneseene\nswweswneswnenwsewnwneneseenw\neesenwseswswnenwswnwnwsewwnwsene\nsewnenenenesenwsewnenwwwse\nwenwwweseeeweswwwnwwe\nwsweesenenewnwwnwsenewsenwwsesesenwne\nneeswseenwwswnwswswnw\nnenwswwsewswnenenewsenwsenwnesesenew\nenewnwewneswsewnwswenweswnenwsenwsw\nsweneswneswneneenwnewenewwneswswnese\nswwesenesewenwneswnwwneseswwne\nenesenwswwswneneswsenwnewswseenwsese\nwnwnesenesenenwwnenwsewesewsesesew\nnenewswnwewswnenesenwnesewesw\neneswnwswnwsenenwnwnwwseeswneewsenese\nneswnwewnwnwseenwseesewsenwsweewe\nwseweeenwnesenwwwswnew"
-- (10,2208)

solve :: String -> (Int, Int)
solve s = (first, second)
  where
    first = fromRight 0 $ S.size <$> tiles
    second = fromRight 0 $ length . flipE 100 <$> tiles
    tilechanges = fmap fromReference <$> input
    input = traverse (runParser parseLine "") $ lines s
    flipE :: Int -> S.Set Direction -> S.Set Direction
    flipE n t
      | n == 0 = t
      | otherwise = flipE (n -1) nxt
      where
        candidates = S.unions $ adjacents <$> S.toList t
        whites = S.difference candidates t
        na x = S.size $ S.intersection t $ adjacents x
        flipToBlack = S.fromList [tile | tile <- S.toList whites, na tile == 2]
        flipToWhite = S.fromList [tile | tile <- S.toList t, let adj = na tile, adj == 0 || adj > 2]
        nxt = S.union flipToBlack $ S.difference t flipToWhite
    tiles = foldl flipTile S.empty <$> tilechanges
      where
        flipTile ts t
          | S.member t ts = S.delete t ts
          | otherwise = S.insert t ts

main :: IO ()
main = readFile "input/day24.txt" >>= print . solve
