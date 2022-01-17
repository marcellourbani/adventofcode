#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main where

import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Set as S

data Category = Weapon | Armor | Ring deriving (Show, Eq, Ord)

data Item = Item {name :: String, _category :: Category, _cost :: Int, _damage :: Int, _armor :: Int} deriving (Show, Eq, Ord)

data Character = Character {_hitPoints :: Int, _cdamage :: Int, _carmor :: Int} deriving (Show, Eq)

parse :: String -> Character
parse i = Character p d a
  where
    [p, d, a] = read . last . words <$> lines i

items =
  S.fromList
    [ Item "Dagger" Weapon 8 4 0,
      Item "Shortsword" Weapon 10 5 0,
      Item "Warhammer" Weapon 25 6 0,
      Item "Longsword" Weapon 40 7 0,
      Item "Greataxe" Weapon 74 8 0,
      Item "Leather" Armor 13 0 1,
      Item "Chainmail" Armor 31 0 2,
      Item "Splintmail" Armor 53 0 3,
      Item "Bandedmail" Armor 75 0 4,
      Item "Platemail" Armor 102 0 5,
      Item "Damage +1" Ring 25 1 0,
      Item "Damage +2" Ring 50 2 0,
      Item "Damage +3" Ring 100 3 0,
      Item "Defense +1" Ring 20 0 1,
      Item "Defense +2" Ring 40 0 2,
      Item "Defense +3" Ring 80 0 3
    ]

survivingTruns :: Character -> Int -> Int
survivingTruns (Character hp _ ar) a = case mod hp tp of
  0 -> div hp tp
  _ -> div hp tp + 1
  where
    tp = max 1 $ a - ar

players :: [(Character, (Int, [Item]))]
players = sortBy (on compare (fst . snd)) $ do
  w <- weapons
  a <- armors
  r <- rings
  let inv = w : a <> r
  return $ mkPlayer inv
  where
    mkPlayer i = (Character 100 (sum $ _damage <$> i) (sum $ _armor <$> i), (sum $ _cost <$> i, i))
    bycat c = S.toList $ S.filter ((== c) . _category) items
    weapons = bycat Weapon
    armors = [] : ((: []) <$> bycat Armor)
    rl = bycat Ring
    rings = [] : [[r] | r <- rl] ++ [[r1, r2] | r1 <- rl, r2 <- rl, r1 < r2]

match :: Character -> Character -> Bool
match enemy player = survivingTruns player (_cdamage enemy) >= survivingTruns enemy (_cdamage player)

-- >>> solve $ parse"Hit Points: 103\nDamage: 9\nArmor: 2"
-- (121,201)

solve :: Character -> (Int, Int)
solve enemy = (p1, p2)
  where
    p1 = fst . snd $ head $ filter (match enemy . fst) players
    p2 = fst . snd $ last $ filter (not . match enemy . fst) players

main :: IO ()
main = readFile "input/day21.txt" >>= print . solve . parse
