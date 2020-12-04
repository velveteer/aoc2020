{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
-- |
-- Module      : Advent.Day4
-- Description : Day 4 Solutions

-- <https://adventofcode.com/2020/day/4>

module Advent.Day4 where

import           Advent.Prelude
import           Data.List      ((\\))

day4a :: IO Int
day4a = load "day4.txt" <&> solveDay4a

day4Example :: [String]
day4Example =
  [ "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
  , "byr:1937 iyr:2017 cid:147 hgt:183cm"
  , ""
  , "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
  , "hcl:#cfa07d byr:1929"
  , ""
  , "hcl:#ae17e1 iyr:2013"
  , "eyr:2024"
  , "ecl:brn pid:760753108 byr:1931"
  , "hgt:179cm"
  , ""
  , "hcl:#cfa07d eyr:2025 pid:166559648"
  , "iyr:2011 ecl:brn hgt:59in"
  ]

requiredKeys :: [String]
requiredKeys = ["byr", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"]

toKeyValue :: String -> (String, String)
toKeyValue (splitOn ":" -> [key, value]) = (key, value)
toKeyValue _ = error "Could not parse key/value pair"

validated :: ((String, String) -> Bool) -> [String] -> Int
validated pred ls = areValid
  where pairsList = toKeyValue <$$> concatMap words <$> splitOn [""] ls
        areValid = foldl' go 0 pairsList
        go !acc pairs =
          case requiredKeys \\ (fst <$> pairs) of
            [] | all pred pairs -> acc + 1
            _                   -> acc

-- | Solve Day 4 Part One
-- Count the number of valid passports - those that have all required fields.
-- >>> solveDay4a day4Example
-- 2
solveDay4a :: [String] -> Int
solveDay4a = validated (const True)

day4b :: IO Int
day4b = load "day4.txt" <&> solveDay4b

day4ExampleTwo :: [String]
day4ExampleTwo =
  -- Invalid
  [ "eyr:1972 cid:100"
  , "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
  , ""
  , "iyr:2019"
  , "hcl:#602927 eyr:1967 hgt:170cm"
  , "ecl:grn pid:012533040 byr:1946"
  , ""
  , "hcl:dab227 iyr:2012"
  , "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
  , ""
  , "hgt:59cm ecl:zzz"
  , "eyr:2038 hcl:74454a iyr:2023"
  , "pid:3556412378 byr:2007"
  , ""
  -- Valid
  , "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980"
  , "hcl:#623a2f"
  , ""
  , "eyr:2029 ecl:blu cid:129 byr:1989"
  , "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
  , ""
  , "hcl:#888785"
  , "hgt:164cm byr:2001 iyr:2015 cid:88"
  , "pid:545766238 ecl:hzl"
  , "eyr:2022"
  , ""
  , "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
  ]

-- | Solve Day 4 Part Two
-- Count the number of valid passports -
-- those that have all required fields and valid values.
-- Continue to treat cid as optional.
-- >>> solveDay4b day4ExampleTwo
-- 4
solveDay4b :: [String] -> Int
solveDay4b = validated isValidPair

eyeColors :: [String]
eyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

isValidPair :: (String, String) -> Bool
isValidPair (key, value)
  | key == "byr"
  , Just v <- readMaybe @Int value
  = v >= 1920 && v <= 2002

  | key == "ecl"
  , Just v <- listToMaybe (words value)
  = v `elem` eyeColors

  | key == "eyr"
  , Just v <- readMaybe @Int value
  = v >= 2020 && v <= 2030

  | key == "hcl"
  , ('#':v) <- value
  = length v == 6 && all (`elem` ['a'..'f'] <> ['0'..'9']) v

  | key == "hgt"
  , (readMaybe @Int -> Just num, unit)
  <- partition isDigit value
  = case unit of
      "cm" -> num >= 150 && num <= 193
      "in" -> num >= 59  && num <= 76
      _    -> False

  | key == "iyr"
  , Just v <- readMaybe @Int value
  = v >= 2010 && v <= 2020

  | key == "pid"
  = length value == 9 && all isDigit value

  | key == "cid" = True
  | otherwise    = False
