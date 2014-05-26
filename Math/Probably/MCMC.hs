{-# LANGUAGE ScopedTypeVariables #-}

module Math.Probably.MCMC (
  -- * Strategies
    continuousSlice
  , discreteSlice
  , hamiltonian
  , mala
  , metropolis
  , nuts
  , nutsDualAveraging
  -- * Interleaving
  , interleave
  , polyInterleave
  , frequency
  , firstWithProb
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
import Strategy.Hamiltonian
import Strategy.MALA
import Strategy.Metropolis
import Strategy.NUTS
import Strategy.NUTSDualAveraging
import Strategy.Slice

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

firstWithProb :: Double -> Transition a -> Transition a -> Transition a
firstWithProb p t0 t1 = do
  s <- lift $ bernoulli p
  if s then t0 else t1

trace :: Int -> Transition t -> Chain t -> Prob (Prob Parameters)
trace n t o = Samples <$> replicateM n t `evalStateT` o

ezMC :: (Chain t -> Prob (Chain t)) -> Transition t
ezMC f = get >>= lift . f >>= put >> gets parameterSpacePosition

polyInterleave :: Transition t1 -> Transition t2 -> Transition (t1,t2)
polyInterleave tr1 tr2 = do
  Chain current0 target val0 (tun1, tun2) <- get 
  let chain1 = Chain current0 target val0 tun1
  
  Chain current1 target1 val1 tun1next <- lift $ execStateT tr1 chain1
  let chain2 = Chain current1 target1 val1 tun2

  (ret, Chain current2 target2 val2 tun2next) <- lift $ runStateT tr2 chain2
  put $ Chain current2 target2 val2 (tun1next, tun2next) 

  return ret

