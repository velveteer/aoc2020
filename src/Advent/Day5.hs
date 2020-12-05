{-# LANGUAGE LambdaCase #-}
-- |
-- Module      : Advent.Day5
-- Description : Day 5 Solutions

-- <https://adventofcode.com/2020/day/5>

module Advent.Day5 where

import           Advent.Prelude
import           Data.List      ((\\))
import           Numeric        (readInt)

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
  fst . head $ readInt 2 (const True) digitToInt (fmap toBin str)

toBin :: Char -> Char
toBin = \case
  'F' -> '0'
  'B' -> '1'
  'L' -> '0'
  'R' -> '1'
  _   -> error "Cannot parse boarding pass"

-- | Solve Day 5 Part Two
-- Get my seat id
solveDay5b :: [String] -> Int
solveDay5b strs = head $ [minimum ids..maximum ids] \\ ids
  where ids = getSeatId <$> strs

day5b :: IO Int
day5b = load "day5.txt" <&> solveDay5b
