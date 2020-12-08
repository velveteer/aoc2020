-- |
-- Module      : Advent.Day8
-- Description : Day 8 Solutions

-- <https://adventofcode.com/2020/day/8>

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Advent.Day8 where

import           Advent.Prelude
import           Data.Attoparsec.Text
import qualified Data.List            as List
import qualified Data.Map.Strict      as Map

day8a :: IO Int
day8a = load "day8.txt" <&> solveDay8a

day8Example :: [String]
day8Example =
  [ "nop +0"
  , "acc +1"
  , "jmp +4"
  , "acc +3"
  , "jmp -3"
  , "acc -99"
  , "acc +1"
  , "jmp -4"
  , "acc +6"
  ]

type Line = (Int, Inst)

data Inst = Nop Int | Acc Int | Jmp Int
  deriving (Eq, Show)

flipInst :: Inst -> Inst
flipInst (Nop int) = Jmp int
flipInst (Jmp int) = Nop int
flipInst (Acc int) = Acc int

asInsts :: [[String]] -> [Inst]
asInsts = fmap asInst

asInst :: [String] -> Inst
asInst [inst, s:(read -> n)] = parseInst inst
  where
    int = if s == '+' then n else negate n
    parseInst = \case
      "nop" -> Nop int
      "acc" -> Acc int
      "jmp" -> Jmp int
      _     -> error "Cannot parse instruction"

interpret :: [Inst] -> Int
interpret prog = go 0 0 []
  where
    go acc n ops
      | n `elem` ops = acc
      | otherwise =
        case prog !! n of
          Acc int -> go (acc + int) (n + 1) (n:ops)
          Jmp int -> go acc (n + int) (n:ops)
          Nop _   -> go acc (n + 1) (n:ops)

-- | Solve Day 8 Part One
--  Immediately before any instruction is executed a second time,
-- what value is in the accumulator?
-- >>> solveDay8a day8Example
-- 5
solveDay8a :: [String] -> Int
solveDay8a = interpret . asInsts . map words

day8b :: IO Int
day8b = load "day8.txt" <&> solveDay8b

-- | Solve Day 8 Part Two
-- Fix the program so that it terminates normally by changing
-- exactly one jmp (to nop) or nop (to jmp).
-- What is the value of the accumulator after the program terminates?
-- >>> solveDay8b day8Example
-- 8
solveDay8b :: [String] -> Int
solveDay8b = reinterpret . asInsts . map words

-- I don't like this brute force approach.
-- There's certainly a better way. Maybe with some graphs.
reinterpret :: [Inst] -> Int
reinterpret = join (go 0 0 0 [])
  where
    go idx acc n ops opgm pgm
      | n `elem` ops =
        go (idx + 1) 0 0 [] opgm (opgm & element idx %~ flipInst)
      | n == length pgm = acc
      | otherwise =
        case pgm !! n of
          Acc int -> go idx (acc + int) (n + 1) (n:ops) opgm pgm
          Jmp int -> go idx acc (n + int) (n:ops) opgm pgm
          Nop _   -> go idx acc (n + 1) (n:ops) opgm pgm
