{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Advent.Day3
-- Description : Day 3 Solutions

-- <https://adventofcode.com/2020/day/3>

module Advent.Day3 where

import Advent.Prelude

day3a :: IO Int
day3a = load "day3.txt" <&> solveDay3a 3 1

day3Example :: [String]
day3Example =
  [ "..##......."
  , "#...#...#.."
  , ".#....#..#."
  , "..#.#...#.#"
  , ".#...##..#."
  , "..#.##....."
  , ".#.#.#....#"
  , ".#........#"
  , "#.##...#..."
  , "#...##....#"
  , ".#..#...#.#"
  ]

-- | Solve Day 3 Part One
-- >>> solveDay3a 3 1 day3Example
-- 7
solveDay3a :: Int -> Int -> [String] -> Int
solveDay3a sx sy ls = go 0 0 0
  where
    go !x !y !acc
      | y >= length ls = acc
      | otherwise =
        case (fmap (concat . repeat) ls !! y) !! x of
          '#' -> go (x + sx) (y + sy) (acc + 1)
          '.' -> go (x + sx) (y + sy) acc
          _   -> error "Invalid map character"

day3b :: IO Int
day3b = load "day3.txt" <&> solveDay3b

slopes :: [(Int, Int)]
slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

-- | Solve Day 3 Part One
-- >>> solveDay3b day3Example
-- 336
solveDay3b :: [String] -> Int
solveDay3b ls = product $ uncurry (flip flip ls . solveDay3a) <$> slopes
