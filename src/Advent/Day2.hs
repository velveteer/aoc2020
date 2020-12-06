{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards  #-}
-- |
-- Module      : Advent.Day2
-- Description : Day 2 Solutions

-- <https://adventofcode.com/2020/day/2>

module Advent.Day2 where

import           Advent.Prelude
import qualified Data.List       as L
import qualified Data.List.Split as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe      as M

day2a :: IO Int
day2a = load "day2.txt" <&> asPolicies <&> solveDay2a

type Password = String

data Policy =
  Policy
    { pVar1 :: Int
    , pVar2 :: Int
    , pCh   :: Char
    } deriving Show

-- | Parse a string into a policy and password product
-- >>> asPolicies ["13-14 f: fffffffnfffvv"]
-- [(Policy {pVar1 = 13, pVar2 = 14, pCh = 'f'},"fffffffnfffvv")]
asPolicies :: [String] -> [(Policy, Password)]
asPolicies = fmap toPolicy
  where
    toPolicy str
      | [range, ch:_, pass] <- words str =
          let [var1,var2] = fmap read . L.splitOn "-" $ range
           in (Policy var1 var2 ch, pass)
      | otherwise = error "Cannot parse password policy"

getValidPassword :: Policy -> Password -> Maybe Password
getValidPassword Policy{pVar1=pMin,pVar2=pMax,pCh} pass = do
  freq <- Map.lookup pCh (histogram pass)
  guard $ freq <= pMax && freq >= pMin
  pure pass

-- | Solve Day 2 Part One
-- >>> solveDay2a [(Policy 1 3 'b', "cdefg"), (Policy 1 3 'a', "abcde")]
-- 1
solveDay2a :: [(Policy, Password)] -> Int
solveDay2a = length . M.mapMaybe (uncurry getValidPassword)

day2b :: IO Int
day2b = load "day2.txt" <&> asPolicies <&> solveDay2b

-- | Solve Day 2 Part Two
-- >>> solveDay2b [(Policy 1 3 'a', "abcde"), (Policy 1 3 'b', "cdefg")]
-- 1
solveDay2b :: [(Policy, Password)] -> Int
solveDay2b = length . M.mapMaybe (uncurry getNewValidPassword)

getNewValidPassword :: Policy -> Password -> Maybe Password
getNewValidPassword Policy{pVar1=pPos1,pVar2=pPos2,pCh} pass = do
  let indices = (+1) <$> L.elemIndices pCh pass
  guard $ length (filter ((||) <$> (pPos1 ==) <*> (pPos2 ==)) indices) == 1
  pure pass
