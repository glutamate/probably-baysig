{-# LANGUAGE ScopedTypeVariables #-}

module Math.Probably.MCMC (metropolis, trace, mala, hamiltonian, nuts) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Math.Probably.Sampler
import Math.Probably.Types
import Numeric.LinearAlgebra
import Strategy.Hamiltonian
import Strategy.MALA
import Strategy.Metropolis
import Strategy.NUTS
-- import Strategy.NUTSDualAveraging
-- import Strategy.Slice

-- interleave :: [Transition a] -> Transition a
-- interleave = foldl1 (>>) 
-- 
-- oneOfRandomly :: [Transition a] -> Transition a
-- oneOfRandomly ts = do
--   j <- lift $ oneOf [0..(length ts - 1)]
--   ts !! j
-- 
-- frequency :: [(Int, Transition a)] -> Transition a
-- frequency xs = lift (oneOf [1..tot]) >>= (`pick` xs) where
--   tot = sum . map fst $ xs
--   pick n ((k, v):vs)
--     | n <= k    = v
--     | otherwise = pick (n - k) vs
-- 
-- firstWithProb :: Double -> Transition a -> Transition a -> Transition a
-- firstWithProb p t0 t1 = do
--   s <- lift $ bernoulli p
--   if s then t0 else t1

trace :: Int -> Transition t -> Chain t -> Prob (Prob Parameters)
trace n t o = Samples <$> replicateM n t `evalStateT` o

