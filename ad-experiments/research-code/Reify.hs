{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Reify where

import Utils

data List a b = Cons a b | Nil deriving (Eq, Ord, Show, Functor)

instance MuRef [a] where
  type DeRef [a]    = List a
  mapDeRef f (x:xs) = Cons x <$> f xs
  mapDeRef f []     = pure Nil
  
ex = 99 : 100 : ex

main = do
  g <- reifyGraph ex
  print $ cse g
