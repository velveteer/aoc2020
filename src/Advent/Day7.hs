-- |
-- Module      : Advent.Day7
-- Description : Day 7 Solutions

-- <https://adventofcode.com/2020/day/7>

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Advent.Day7 where

import           Advent.Prelude
import           Data.Attoparsec.Text
import qualified Data.List            as List
import qualified Data.Map.Strict      as Map

day7a :: IO Int
day7a = load "day7.txt" <&> solveDay7a

day7Example :: [String]
day7Example =
  [ "light red bags contain 1 bright white bag, 2 muted yellow bags."
  , "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
  , "bright white bags contain 1 shiny gold bag."
  , "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
  , "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
  , "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
  , "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
  , "faded blue bags contain no other bags."
  , "dotted black bags contain no other bags."
  ]

-- | Solve Day 7 Part One
-- How many bag colors can eventually contain at least one shiny gold bag?
-- >>> solveDay7a day7Example
-- 4
solveDay7a :: [String] -> Int
solveDay7a ls =
  let bags = Map.fromList . rights . fmap (parseOnly parseBag . pack) $ ls
    in sum $ containsColor "shinygold" bags <$> Map.keys bags

type Bag = (Text, [(Text, Int)])

parseBag :: Parser Bag
parseBag = (,) <$> (parseColor <* string " bags ") <*> parseContains

parseColor :: Parser Text
parseColor =
  pack <$>
    (many1' letter >>= \adj -> fmap (adj <>) (space *> many1' letter))

parseContains :: Parser [(Text, Int)]
parseContains = string "contain " *>
  sepBy
    (flip (,) <$> (decimal <* space) <*> parseColor <* space <* many letter)
    (string ", ")

containsColor :: Text -> Map Text [(Text, Int)] -> Text -> Int
containsColor color bags name =
  case Map.lookup name bags of
    Just (fmap fst -> children)
      | color `elem` children -> 1
      | otherwise ->
        if 1 `elem` (containsColor color bags <$> children)
          then 1
          else 0
    _ -> 0

getAllInside :: Text -> Map Text [(Text, Int)] -> Int
getAllInside name map =
  case Map.lookup name map of
    Nothing -> 0
    Just els ->
      sum ((\(c, n) -> n + (n * getAllInside c map)) <$> els)

day7bExample :: [String]
day7bExample =
  [ "shiny gold bags contain 2 dark red bags."
  , "dark red bags contain 2 dark orange bags."
  , "dark orange bags contain 2 dark yellow bags."
  , "dark yellow bags contain 2 dark green bags."
  , "dark green bags contain 2 dark blue bags."
  , "dark blue bags contain 2 dark violet bags."
  , "dark violet bags contain no other bags."
  ]

day7b :: IO Int
day7b = load "day7.txt" <&> solveDay7b

-- | Solve Day 7 Part Two
-- How many individual bags are required inside your single shiny gold bag?
-- >>> solveDay7b day7bExample
-- 126
solveDay7b :: [String] -> Int
solveDay7b =
  getAllInside "shinygold" . Map.fromList . rights . fmap (parseOnly parseBag . pack)
