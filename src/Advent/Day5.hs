-- |
-- Module      : Advent.Day5
-- Description : Day 5 Solutions

-- <https://adventofcode.com/2020/day/5>

module Advent.Day5 where

import           Advent.Prelude
import           Data.List ((\\))

day5a :: IO Int
day5a = load "day5.txt" <&> solveDay5a

-- | Solve Day 5 Part One
-- What is the highest seat ID on a boarding pass?
solveDay5a :: [String] -> Int
solveDay5a = maximum . fmap getSeatId

-- | Get seat id from a boarding pass.
-- >>> getSeatId "FBFBBFFRLR"
-- 357
getSeatId :: String -> Int
getSeatId str =
  let ([r], [c]) = foldl' go ([0..127], [0..7]) str in r * 8 + c
  where
    go (r, c) ch =
      case ch of
        'F' -> (take (length r `div` 2) r, c)
        'B' -> (drop (length r `div` 2) r, c)
        'R' -> (r, drop (length c `div` 2) c)
        'L' -> (r, take (length c `div` 2) c)
        _   -> error "Cannot parse boarding pass"

-- | Solve Day 5 Part Two
-- Get my seat id
solveDay5b :: [String] -> Int
solveDay5b strs = head $ [minimum ids..maximum ids] \\ ids
  where ids = getSeatId <$> strs

day5b :: IO Int
day5b = load "day5.txt" <&> solveDay5b
