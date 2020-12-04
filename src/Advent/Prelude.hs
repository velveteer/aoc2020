module Advent.Prelude
  ( module Export
  , load
  , (<&>)
  , tails
  ) where

import Prelude as Export
import Control.Monad as Export
import Data.Functor ((<&>))
import Data.List (tails)
import Data.Maybe as Export

load :: FilePath -> IO [String]
load = fmap lines . readFile . ("text/" <>)
