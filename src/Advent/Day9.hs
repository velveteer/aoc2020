-- |
-- Module      : Advent.Day9
-- Description : Day 9 Solutions

-- <https://adventofcode.com/2020/day/9>

module Advent.Day9 where

import           Advent.Prelude
import           Data.Attoparsec.Text
import qualified Data.List            as List
import qualified Data.Map.Strict      as Map

day9a :: IO Int
day9a = load "day9.txt" <&> asInts <&> solveDay9a 25

day9b :: IO Int
day9b = do
  ints <- load "day9.txt" <&> asInts
  let pt1 = solveDay9a 25 ints
  pure $ solveDay9b pt1 ints

day9Example :: [String]
day9Example =
  [ "35"
  , "20"
  , "15"
  , "25"
  , "47"
  , "40"
  , "62"
  , "55"
  , "65"
  , "95"
  , "102"
  , "117"
  , "150"
  , "182"
  , "127"
  , "219"
  , "299"
  , "277"
  , "309"
  , "576"
  ]

-- | Solve Day 9 Part One
-- Find the first number in the list (after the preamble)
-- which is not the sum of two of the 25 numbers before it.
-- >>> solveDay9a 5 (asInts day9Example)
-- 127
solveDay9a :: Int -> [Int] -> Int
solveDay9a n ints = go (List.splitAt n ints)
  where
    getSums l = [x + y | (x:ys) <- tails l, y <- ys]
    go (lastN, int:xs) =
      if int `elem` getSums lastN
      then go (tail lastN <> [int], xs)
      else int

-- | Solve Day 9 part Two
-- Adding up all of the numbers from 15 through 40 produces the invalid number from step 1, 127.
-- To find the encryption weakness, add together the smallest and largest number in this contiguous range;
-- in this example, these are 15 and 47, producing 62.
-- >>> solveDay9b 127 (asInts day9Example)
-- 62
solveDay9b :: Int -> [Int] -> Int
solveDay9b n ints = ((+) <$> minimum <*> maximum)
                  . head
                  . List.sortOn (Down . length)
                  $ fmap (go []) (tails ints)
  where
    go acc (x:xs)
      | sum (x:acc) < n = go (x:acc) xs
      | sum (x:acc) > n = []
      | otherwise       = x:acc
    go acc []           = acc
