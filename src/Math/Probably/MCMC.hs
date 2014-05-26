{-# LANGUAGE ScopedTypeVariables #-}

-- | Utilities for interleaving transition operators and running Markov chains.

module Math.Probably.MCMC (
  -- * Strategies
    module Strategy
  -- * Interleaving
  , interleave
  , polyInterleave
  , frequency
  , oneOfRandomly
  -- * Other
  , ezMC
  , trace
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Math.Probably.Sampler
import Math.Probably.Types
import Strategy.Hamiltonian       as Strategy
import Strategy.MALA              as Strategy
import Strategy.Metropolis        as Strategy
import Strategy.NUTS              as Strategy
import Strategy.NUTSDualAveraging as Strategy
import Strategy.Slice             as Strategy

-- | Interleave a list of transitions together.
--
--   > interleave [metropolis Nothing, mala Nothing]
--   -- :: Transition Double
interleave :: [Transition a] -> Transition a
interleave = foldl1 (>>) 

-- | Select a transition operator from a list uniformly at random.
oneOfRandomly :: [Transition a] -> Transition a
oneOfRandomly ts = do
  j <- lift $ oneOf [0..(length ts - 1)]
  ts !! j

-- | Select a transition operator from a list according to supplied
--   frequencies.  Mimics the similar function from QuickCheck.
--
--   > frequency [(9, continuousSlice Nothing), (1, nutsDualAveraging Nothing)]
--   -- probably a slice transition
frequency :: [(Int, Transition a)] -> Transition a
frequency xs = lift (oneOf [1..tot]) >>= (`pick` xs) where
  tot = sum . map fst $ xs
  pick n ((k, v):vs)
    | n <= k    = v
    | otherwise = pick (n - k) vs
  pick _ [] = error "frequency: empty list"

-- | Heterogeneous type interleaving.
polyInterleave :: Transition t1 -> Transition t2 -> Transition (t1,t2)
polyInterleave tr1 tr2 = do
  Chain current0 target val0 (tun1, tun2) <- get 
  let chain1 = Chain current0 target val0 tun1
  
  Chain current1 target1 val1 tun1next <- lift $ execStateT tr1 chain1
  let chain2 = Chain current1 target1 val1 tun2

  (ret, Chain current2 target2 val2 tun2next) <- lift $ runStateT tr2 chain2
  put $ Chain current2 target2 val2 (tun1next, tun2next) 
  return ret

-- | Construct a transition from a supplied function.
ezMC :: (Chain t -> Prob (Chain t)) -> Transition t
ezMC f = get >>= lift . f >>= put >> gets parameterSpacePosition

-- | Run a Markov Chain.
trace :: Int -> Transition t -> Chain t -> Prob (Prob Parameters)
trace n t o = Samples <$> replicateM n t `evalStateT` o

