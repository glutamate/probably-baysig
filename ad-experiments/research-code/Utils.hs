
module Utils (
    square
  , sumSquares
  , vgrad
  , vgrad'
  , module Control.Applicative
  , module Data.Data
  , module Data.Generics.Uniplate.Data
  , module Data.Reify
  , module Data.Reify.Graph.CSE
  , module Data.Foldable
  , module Data.Maybe
  , module Data.Traversable
  , module Numeric.AD.Mode
  , module Numeric.AD.Mode.Kahn
  ) where

import Control.Applicative
import Data.Data
import Data.Generics.Uniplate.Data
import Data.Foldable
import Data.Maybe
import Data.Reify
import Data.Reify.Graph.CSE
import Data.Traversable
import Numeric.AD (vgrad, vgrad')
import Numeric.AD.Mode
import Numeric.AD.Mode.Kahn

square :: Num a => a -> a
square x = x * x

sumSquares :: Num a => [a] -> a
sumSquares [x, y] = square x + square y

