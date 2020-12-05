module Advent.Prelude
  ( module Export
  , (<$$>)
  , load
  ) where

import           Prelude             as Export hiding (pred)

import           Control.Applicative as Export
import           Control.Monad       as Export
import           Data.Char           as Export (digitToInt, isDigit)
import           Data.Either         as Export
import           Data.Functor        as Export ((<&>))
import           Data.List           as Export (foldl', partition, tails)
import           Data.List.Split     as Export (splitOn)
import           Data.Maybe          as Export
import           Data.Text           as Export (Text, pack)
import           Text.Read           as Export (readMaybe)

load :: FilePath -> IO [String]
load = fmap lines . readFile . ("text/" <>)

infixl 1 <$$>
(<$$>) :: Functor f => (a -> b) -> f (f a) -> f (f b)
f <$$> b = (fmap . fmap) f b
