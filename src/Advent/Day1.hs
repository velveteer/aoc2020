-- |
-- Module      : Advent.Day1
-- Description : Day 1 Solutions

-- <https://adventofcode.com/2020/day/1>

module Advent.Day1 where

import           Advent.Prelude

day1a :: IO (Maybe Int)
day1a = load "day1.txt" <&> asInts <&> solveDay1a

day1Example :: [Int]
day1Example = [1721, 979, 366, 299, 675, 1456]

-- | Solve Day 1 Part One
-- >>> solveDay1a day1Example
-- Just 514579
solveDay1a :: [Int] -> Maybe Int
solveDay1a ints = listToMaybe solution
  where
    -- For each suffix of the list, keep a reference to the head (x) and iterate over the tail
    -- For each element of the suffix's tail, sum it with the head (x)
    -- The tails law ensures the first element is the original list: head (tails xs) = xs
    -- My original approach used the cartesian product of the list with itself,
    -- which produces duplicates and mirrored pairs.
    -- While that does work it is wasteful, tails lets us compute only what we need.
    solution = do
      x:xs <- tails ints
      y    <- xs
      guard $ x + y == 2020
      pure  $ x * y

day1b :: IO (Maybe Int)
day1b = load "day1.txt" <&> asInts <&> solveDay1b

-- | Solve Day 1 Part Two
-- >>> solveDay1b day1Example
-- Just 241861950
solveDay1b :: [Int] -> Maybe Int
solveDay1b ints = listToMaybe solution
  where
    solution = do
      x:xs <- tails ints
      y:ys <- tails xs
      z    <- ys
      guard $ x + y + z == 2020
      pure  $ x * y * z
