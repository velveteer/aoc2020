module Advent.Prelude
  ( module Export
  , (<$$>)
  , load
  , asInts
  , histogram
  ) where

import           Prelude             as Export hiding (pred)

import           Control.Applicative as Export
import           Control.Arrow       as Export
import           Control.Lens        as Export
import           Control.Monad       as Export
import           Data.Char           as Export (digitToInt, isDigit)
import           Data.Either         as Export
import           Data.Functor        as Export ((<&>))
import           Data.List           as Export (foldl', foldr1, nubBy, partition, tails)
import           Data.List.Split     as Export (splitOn)
import           Data.Map.Strict     as Export (Map)
import qualified Data.Map.Strict     as Map
import           Data.Maybe          as Export
import Data.Ord as Export (Down(..), comparing)
import           Data.Set            as Export (Set)
import           Data.Text           as Export (Text, pack, unpack)
import           Text.Read           as Export (readMaybe)

load :: FilePath -> IO [String]
load = fmap lines . readFile . ("text/" <>)

infixl 1 <$$>
(<$$>) :: Functor f => (a -> b) -> f (f a) -> f (f b)
f <$$> b = (fmap . fmap) f b

histogram :: String -> Map Char Int
histogram str = Map.fromListWith (+) (zip str $ repeat 1)

asInts :: [String] -> [Int]
asInts = fmap read

