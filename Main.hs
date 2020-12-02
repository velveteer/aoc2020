module Main where

import           Control.Monad (guard)
import           Data.Functor  ((<&>))
import           Data.Function ((&))
import qualified Data.List as L
import qualified Data.Maybe as M

main :: IO ()
main = do
  print =<< day1a
  print =<< day1b

load :: FilePath -> IO [String]
load = fmap lines . readFile . ("text/" <>)

asInts :: [String] -> [Int]
asInts = fmap read

-- =========================
-- Day 1 Part One
-- =========================

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

day1a :: IO (Maybe Int)
day1a = load "day1.txt" <&> asInts <&> solveDay1a

solveDay1a :: [Int] -> Maybe Int
solveDay1a ints = M.listToMaybe solution
  where
    solution = do
      x:xs <- L.tails ints
      y    <- xs
      guard $ x + y == 2020
      pure  $ x * y

-- =========================
-- Day 1 Part Two
-- =========================
-- The Elves in accounting are thankful for your help; one of them even
-- offers you a starfish coin they had left over from a past vacation.
-- They offer you a second one if you can find three numbers in your
-- expense report that meet the same criteria.

-- Using the above example again, the three entries that sum to 2020 are 979,
-- 366, and 675. Multiplying them together produces the answer, 241861950.

-- In your expense report, what is the product of the three entries that sum to 2020?

day1b :: IO (Maybe Int)
day1b = load "day1.txt" <&> asInts <&> solveDay1b

solveDay1b :: [Int] -> Maybe Int
solveDay1b ints = M.listToMaybe solution
  where
    solution = do
      x:xs <- L.tails ints
      y:ys <- L.tails xs
      z    <- ys
      guard $ x + y + z == 2020
      pure  $ x * y * z
