{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards  #-}

module Main where

import           Control.Monad (guard)
import           Data.Bifunctor (first)
import           Data.Char (digitToInt)
import           Data.Functor  ((<&>))
import           Data.Function ((&))
import qualified Data.List as L
import qualified Data.List.Split as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as M
import           Data.Traversable (for)

main :: IO ()
main = do
  print =<< day1a
  print =<< day1b
  print =<< day2a
  print =<< day2b
  print =<< day3a
  print =<< day3b

load :: FilePath -> IO [String]
load = fmap lines . readFile . ("text/" <>)

asInts :: [String] -> [Int]
asInts = fmap read

-- =======================
-- Day 1: Report Repair --
-- =======================

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

-- =================
-- Day 1 Part Two --
-- =================
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

-- =============================
-- Day 2: Password Philosophy --
-- =============================
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

-- =================
-- Day 2 Part Two --
-- =================
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

-- =============================
-- Day 3: Toboggan Trajectory --
-- =============================
-- With the toboggan login problems resolved, you set off toward the airport.
-- While travel by toboggan might be easy, it's certainly not safe:
-- there's very minimal steering and the area is covered in trees.
-- You'll need to see which angles will take you near the fewest trees.

-- Due to the local geology, trees in this area only grow on exact
-- integer coordinates in a grid. You make a map (your puzzle input)
-- of the open squares (.) and trees (#) you can see. For example:

-- ..##.......
-- #...#...#..
-- .#....#..#.
-- ..#.#...#.#
-- .#...##..#.
-- ..#.##.....
-- .#.#.#....#
-- .#........#
-- #.##...#...
-- #...##....#
-- .#..#...#.#
-- These aren't the only trees, though; due to something you read about once
-- involving arboreal genetics and biome stability,
-- the same pattern repeats to the right many times:

-- ..##.........##.........##.........##.........##.........##.......  --->
-- #...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
-- .#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
-- ..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
-- .#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
-- ..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....  --->
-- .#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
-- .#........#.#........#.#........#.#........#.#........#.#........#
-- #.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
-- #...##....##...##....##...##....##...##....##...##....##...##....#
-- .#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
-- You start on the open square (.) in the top-left corner and need to reach the
-- bottom (below the bottom-most row on your map).

-- The toboggan can only follow a few specific slopes (you opted for a
-- cheaper model that prefers rational numbers); start by counting all the trees you
-- would encounter for the slope right 3, down 1:

-- From your starting position at the top-left, check the position that
-- is right 3 and down 1. Then, check the position that is right 3 and
-- down 1 from there, and so on until you go past the bottom of the map.

-- The locations you'd check in the above example are marked here with
-- O where there was an open square and X where there was a tree:

-- ..##.........##.........##.........##.........##.........##.......  --->
-- #..O#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
-- .#....X..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
-- ..#.#...#O#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
-- .#...##..#..X...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
-- ..#.##.......#.X#.......#.##.......#.##.......#.##.......#.##.....  --->
-- .#.#.#....#.#.#.#.O..#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
-- .#........#.#........X.#........#.#........#.#........#.#........#
-- #.##...#...#.##...#...#.X#...#...#.##...#...#.##...#...#.##...#...
-- #...##....##...##....##...#X....##...##....##...##....##...##....#
-- .#..#...#.#.#..#...#.#.#..#...X.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
-- In this example, traversing the map using this slope would cause you to encounter 7 trees.

-- Starting at the top-left corner of your map and following a slope of right 3 and down 1,
-- how many trees would you encounter?

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
    go x y acc
      | y >= length ls = acc
      | otherwise =
        case (fmap (concat . repeat) ls !! y) !! x of
          '#' -> go (x + sx) (y + sy) (acc + 1)
          '.' -> go (x + sx) (y + sy) acc
          _   -> error "Invalid map character"

-- =================
-- Day 3 Part Two --
-- =================
-- Time to check the rest of the slopes - you need to minimize the
-- probability of a sudden arboreal stop, after all.

-- Determine the number of trees you would encounter if,
-- for each of the following slopes, you start at the top-left corner
-- and traverse the map all the way to the bottom:

-- Right 1, down 1.
-- Right 3, down 1. (This is the slope you already checked.)
-- Right 5, down 1.
-- Right 7, down 1.
-- Right 1, down 2.
-- In the above example, these slopes would find 2, 7, 3, 4, and 2 tree(s) respectively;
-- multiplied together, these produce the answer 336.

-- What do you get if you multiply together the number of trees
-- encountered on each of the listed slopes?

day3b :: IO Int
day3b = load "day3.txt" <&> solveDay3b

slopes :: [(Int, Int)]
slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

-- | Solve Day 3 Part One
-- >>> solveDay3b day3Example
-- 336
solveDay3b :: [String] -> Int
solveDay3b ls = product $ uncurry (flip flip ls . solveDay3a) <$> slopes
