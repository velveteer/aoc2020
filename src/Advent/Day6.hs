-- |
-- Module      : Advent.Day6
-- Description : Day 6 Solutions

-- <https://adventofcode.com/2020/day/6>

module Advent.Day6 where

import           Advent.Prelude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

day6a :: IO Int
day6a = load "day6.txt" <&> solveDay6a

day6Example :: [String]
day6Example =
  [ "abc"
  , ""
  , "a"
  , "b"
  , "c"
  , ""
  , "ab"
  , "ac"
  , ""
  , "a"
  , "a"
  , "a"
  , "a"
  , ""
  , "b"
  ]

-- | Solve Day 6 Part One
-- For each group, count the number of questions to which anyone answered "yes".
-- What is the sum of those counts?
-- >>> solveDay6a day6Example
-- 11
solveDay6a :: [String] -> Int
solveDay6a =
      splitOn [""]
  >>> fmap (Set.size . Set.fromList . concat)
  >>> sum

day6b :: IO Int
day6b = load "day6.txt" <&> solveDay6b

-- | Solve Day 6 Part Two
-- For each group, count the number of questions to which anyone answered "yes".
-- What is the sum of those counts?
-- >>> solveDay6b day6Example
-- 6
solveDay6b :: [String] -> Int
solveDay6b =
      splitOn [""]
  >>> fmap ((histogram . concat &&& length)
    >>> ((\(m, c) -> Map.filter (== c) m)
      >>> Map.size))
  >>> sum
