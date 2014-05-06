{-# LANGUAGE ScopedTypeVariables #-}

module Math.Probably.MCMC where

import Control.Monad
import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Math.Probably.Sampler
import Math.Probably.Types
import Numeric.LinearAlgebra

interleave :: [Transition a] -> Transition a
interleave = foldl1 (>>) 

oneOfRandomly :: [Transition a] -> Transition a
oneOfRandomly ts = do
  j <- lift $ oneOf [0..(length ts - 1)]
  ts !! j

frequency :: [(Int, Transition a)] -> Transition a
frequency xs = lift (oneOf [1..tot]) >>= (`pick` xs) where
  tot = sum . map fst $ xs
  pick n ((k, v):vs)
    | n <= k    = v
    | otherwise = pick (n - k) vs

firstWithProb
  :: Double
  -> Transition a
  -> Transition a
  -> Transition a
firstWithProb p t0 t1 = do
  s <- lift $ bernoulli p
  if s then t0 else t1

trace
  :: Int
  -> Transition a
  -> Chain a
  -> Prob (Prob (Vector a))
trace n t o = do
  zs <- replicateM n t `evalStateT` o
  return $ Samples zs

