#!/usr/bin/env stack
-- stack --resolver lts-10.2 script

module Main where

import Data.List.Split
import Data.Map (Map, fromList, member, (!))
import Text.Regex.TDFA

sample = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"

-- >>> toPair $ splitOn ":" "abc:pippo"
-- ("abc","pippo")
toPair :: [b] -> (b, b)
toPair [x, y] = (x, y)

mandFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] -- ,"cid" ]

isValid :: Map String String -> Bool
isValid m = and [member k m | k <- mandFields]

isReallyValid :: Map String String -> Bool
isReallyValid p =
  isValid p
    && year > 1920
    && year < 2002
    && issue > 2010
    && issue < 2020
    && exp > 2010
    && exp < 2020
  where
    year = read $ p ! "byr"
    issue = read $ p ! "iyr"
    exp = read $ p ! "eyr"

-- >>> parseMaps sample
-- [fromList [("byr","1937"),("cid","147"),("ecl","gry"),("eyr","2020"),("hcl","#fffffd"),("hgt","183cm"),("iyr","2017"),("pid","860033327")],fromList [("byr","1929"),("cid","350"),("ecl","amb"),("eyr","2023"),("hcl","#cfa07d"),("iyr","2013"),("pid","028048884")],fromList [("byr","1931"),("ecl","brn"),("eyr","2024"),("hcl","#ae17e1"),("hgt","179cm"),("iyr","2013"),("pid","760753108")],fromList [("ecl","brn"),("eyr","2025"),("hcl","#cfa07d"),("hgt","59in"),("iyr","2011"),("pid","166559648")]]
parseMaps :: [Char] -> [Map [Char] [Char]]
parseMaps = fmap (fromList . fmap (toPair . splitOn ":") . words) . splitOn "\n\n"

-- >>> solve $ parseMaps sample
-- (2,-1)
solve :: [Map String String] -> (Int, Int)
solve passports = (length validPassports, -1)
  where
    validPassports = filter isValid passports

main :: IO ()
main = interact $ show . solve . parseMaps
