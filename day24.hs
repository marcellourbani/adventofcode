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

data Direction = Direction {dirE :: Int, dirN :: Int} deriving (Show, Eq, Ord)

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

-- >>> solve "sesenwnenenewseeswwswswwnenewsewsw\nneeenesenwnwwswnenewnwwsewnenwseswesw\nseswneswswsenwwnwse\nnwnwneseeswswnenewneswwnewseswneseene\nswweswneswnenwsewnwneneseenw\neesenwseswswnenwswnwnwsewwnwsene\nsewnenenenesenwsewnenwwwse\nwenwwweseeeweswwwnwwe\nwsweesenenewnwwnwsenewsenwwsesesenwne\nneeswseenwwswnwswswnw\nnenwswwsewswnenenewsenwsenwnesesenew\nenewnwewneswsewnwswenweswnenwsenwsw\nsweneswneswneneenwnewenewwneswswnese\nswwesenesewenwneswnwwneseswwne\nenesenwswwswneneswsenwnewswseenwsese\nwnwnesenesenenwwnenwsewesewsesesew\nnenewswnwewswnenesenwnesewesw\neneswnwswnwsenenwnwnwwseeswneewsenese\nneswnwewnwnwseenwseesewsenwsweewe\nwseweeenwnesenwwwswnew"
-- (10,1)

solve :: String -> (Int, Int)
solve s = (first, second)
  where
    first = fromRight 0 $ S.size <$> tiles
    second = 1
    tilechanges = fmap fromReference <$> input
    input = traverse (runParser parseLine "") $ lines s
    tiles = foldl flipTile S.empty <$> tilechanges
      where
        flipTile ts t
          | S.member t ts = S.delete t ts
          | otherwise = S.insert t ts

main :: IO ()
main = readFile "input/day24.txt" >>= print . solve
