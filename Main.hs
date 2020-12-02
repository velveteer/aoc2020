{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards  #-}

module Main where

import           Control.Monad (guard)
import           Data.Char (digitToInt)
import           Data.Functor  ((<&>))
import           Data.Function ((&))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List as L
import qualified Data.List.Split as L
import qualified Data.Maybe as M

main :: IO ()
main = do
  print =<< day1a
  print =<< day1b
  print =<< day2a
  print =<< day2b

load :: FilePath -> IO [String]
load = fmap lines . readFile . ("text/" <>)

asInts :: [String] -> [Int]
asInts = fmap read

-- ===============================
-- Day 1: Report Repair
-- ===============================

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

day1Example :: [Int]
day1Example = [1721, 979, 366, 299, 675, 1456]

-- | Solve Day 1 Part One
-- >>> solveDay1a day1Example
-- Just 514579
solveDay1a :: [Int] -> Maybe Int
solveDay1a ints = M.listToMaybe solution
  where
    -- For each suffix of the list, keep a reference to the head (x) and iterate over the tail
    -- For each element of the suffix's tail, sum it with the head (x)
    -- The tails law ensures the first element is the original list: head (tails xs) = xs
    -- My original approach used the cartesian product of the list with itself,
    -- which produces duplicates and mirrored pairs.
    -- While that does work it is wasteful, tails lets us compute only what we need.
    solution = do
      x:xs <- L.tails ints
      y    <- xs
      guard $ x + y == 2020
      pure  $ x * y

-- ===============================
-- Day 1 Part Two
-- ===============================
-- The Elves in accounting are thankful for your help; one of them even
-- offers you a starfish coin they had left over from a past vacation.
-- They offer you a second one if you can find three numbers in your
-- expense report that meet the same criteria.

-- Using the above example again, the three entries that sum to 2020 are 979,
-- 366, and 675. Multiplying them together produces the answer, 241861950.

-- In your expense report, what is the product of the three entries that sum to 2020?

day1b :: IO (Maybe Int)
day1b = load "day1.txt" <&> asInts <&> solveDay1b

-- | Solve Day 1 Part Two
-- >>> solveDay1b day1Example
-- Just 241861950
solveDay1b :: [Int] -> Maybe Int
solveDay1b ints = M.listToMaybe solution
  where
    solution = do
      x:xs <- L.tails ints
      y:ys <- L.tails xs
      z    <- ys
      guard $ x + y + z == 2020
      pure  $ x * y * z

-- ===============================
-- Day 2: Password Philosophy
-- ===============================
-- Your flight departs in a few days from the coastal airport; the easiest way
-- down to the coast from here is via toboggan.

-- The shopkeeper at the North Pole Toboggan Rental Shop is having a bad day.
-- "Something's wrong with our computers; we can't log in!" You ask if you can take a look.

-- Their password database seems to be a little corrupted: some of the
-- passwords wouldn't have been allowed by the Official Toboggan Corporate Policy
-- that was in effect when they were chosen.

-- To try to debug the problem, they have created a list (your puzzle input) of passwords
-- (according to the corrupted database) and the corporate policy when that password was set.

-- For example, suppose you have the following list:

-- 1-3 a: abcde
-- 1-3 b: cdefg
-- 2-9 c: ccccccccc
-- Each line gives the password policy and then the password.
-- The password policy indicates the lowest and highest number of times a given letter
-- must appear for the password to be valid.
-- For example, 1-3 a means that the password must contain a at least 1 time and at most 3 times.

-- In the above example, 2 passwords are valid. The middle password, cdefg, is not;
-- it contains no instances of b, but needs at least 1. The first and third passwords are valid:
-- they contain one a or nine c, both within the limits of their respective policies.

-- How many passwords are valid according to their policies?

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

histogram :: String -> Map Char Int
histogram str = Map.fromListWith (+) (zip str $ repeat 1)

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

-- ===============================
-- Day 2 Part Two
-- ===============================
-- While it appears you validated the passwords correctly, they don't seem to be what
-- the Official Toboggan Corporate Authentication System is expecting.

-- The shopkeeper suddenly realizes that he just accidentally explained the password
-- policy rules from his old job at the sled rental place down the street!
-- The Official Toboggan Corporate Policy actually works a little differently.

-- Each policy actually describes two positions in the password,
-- where 1 means the first character, 2 means the second character, and so on.
-- (Be careful; Toboggan Corporate Policies have no concept of "index zero"!)
-- Exactly one of these positions must contain the given letter.
-- Other occurrences of the letter are irrelevant for the purposes of policy enforcement.

-- Given the same example list from above:

-- 1-3 a: abcde is valid: position 1 contains a and position 3 does not.
-- 1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
-- 2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.
-- How many passwords are valid according to the new interpretation of the policies?

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

