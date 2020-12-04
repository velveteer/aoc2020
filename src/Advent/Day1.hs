-- |
-- Module      : Advent.Day1
-- Description : Day 1 Solutions

-- ============================
--     Day 1: Report Repair
-- ============================

-- Before you leave, the Elves in accounting just need you to fix your expense report
-- (your puzzle input); apparently, something isn't quite adding up.

-- Specifically, they need you to find the two entries that sum to 2020 and then
-- multiply those two numbers together.

-- For example, suppose your expense report contained the following:

-- 1721
-- 979
-- 366
-- 299
-- 675
-- 1456
-- In this list, the two entries that sum to 2020 are 1721 and 299.
-- Multiplying them together produces 1721 * 299 = 514579, so the correct answer is 514579.

-- Of course, your expense report is much larger. Find the two entries that sum to 2020;
-- what do you get if you multiply them together?

-- ============================
--           Part Two
-- ============================

-- The Elves in accounting are thankful for your help; one of them even
-- offers you a starfish coin they had left over from a past vacation.
-- They offer you a second one if you can find three numbers in your
-- expense report that meet the same criteria.

-- Using the above example again, the three entries that sum to 2020 are 979,
-- 366, and 675. Multiplying them together produces the answer, 241861950.

-- In your expense report, what is the product of the three entries that sum to 2020?

module Advent.Day1 where

import Advent.Prelude

asInts :: [String] -> [Int]
asInts = fmap read

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
